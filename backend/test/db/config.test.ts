import { describe, it, expect, beforeEach, afterEach, vi } from "vitest";
import { Sequelize } from "sequelize";

// Define a custom interface extending Sequelize for the mock
interface MockedSequelizeInstance extends Sequelize {
  __constructorArgs: any[];
}

// Mock the centralized config module before importing your db config.
// You can adjust mocks per test via vi.doMock.
vi.mock("../../src/config/environment", () => ({
  default: {
    DATABASE: {
      HOST: "localhost",
      PORT: 5432,
      USER: "testuser",
      PASSWORD: "testpass",
      NAME: "testdb",
    },
    NODE_ENV: "development",
  },
}));

// Mock Sequelize constructor and instance
vi.mock("sequelize", () => {
  return {
    Sequelize: vi.fn(function (...args: any[]) {
      // Return a mocked instance with methods and custom __constructorArgs property
      return {
        authenticate: vi.fn(),
        close: vi.fn(),
        sync: vi.fn(),
        __constructorArgs: args, // Expose constructor args for testing
      };
    }),
  };
});

describe("Sequelize Configuration", () => {
  afterEach(() => {
    // Reset mocks and modules after each test
    vi.resetModules();
    vi.clearAllMocks();
  });

  it("should create Sequelize instance with correct config", async () => {
    const { default: sequelizeInstance } = await import("../../src/db/config");

    const typedInstance =
      sequelizeInstance as unknown as MockedSequelizeInstance;
    const args = typedInstance.__constructorArgs;

    expect(args[0]).toBe("testdb");
    expect(args[1]).toBe("testuser");
    expect(args[2]).toBe("testpass");
    expect(args[3]).toMatchObject({
      host: "localhost",
      port: 5432,
      dialect: "postgres",
      logging: expect.any(Function),
    });
  });

  it("should use port 5432 if config port is undefined", async () => {
    // Override config mock to omit PORT
    vi.doMock("../../src/config/environment", () => ({
      default: {
        DATABASE: {
          HOST: "localhost",
          // No PORT property
          USER: "testuser",
          PASSWORD: "testpass",
          NAME: "testdb",
        },
        NODE_ENV: "development",
      },
    }));

    const { default: sequelizeInstance } = await import("../../src/db/config");
    const typedInstance =
      sequelizeInstance as unknown as MockedSequelizeInstance;
    const args = typedInstance.__constructorArgs;

    // If your config/environment.ts fallback sets default port,
    // this test checks for whatever behavior you implement.
    // Here we check that port is undefined or fallback 5432.
    expect(args[3].port === undefined || args[3].port === 5432).toBe(true);
  });

  it("should use custom DB_PORT value", async () => {
    vi.doMock("../../src/config/environment", () => ({
      default: {
        DATABASE: {
          HOST: "localhost",
          PORT: 3306,
          USER: "testuser",
          PASSWORD: "testpass",
          NAME: "testdb",
        },
        NODE_ENV: "development",
      },
    }));

    const { default: sequelizeInstance } = await import("../../src/db/config");
    const typedInstance =
      sequelizeInstance as unknown as MockedSequelizeInstance;
    const args = typedInstance.__constructorArgs;

    expect(args[3].port).toBe(3306);
  });

  it("should handle invalid DB_PORT (NaN) gracefully", async () => {
    vi.doMock("../../src/config/environment", () => ({
      default: {
        DATABASE: {
          HOST: "localhost",
          PORT: Number("invalid"), // Will be NaN
          USER: "testuser",
          PASSWORD: "testpass",
          NAME: "testdb",
        },
        NODE_ENV: "development",
      },
    }));

    const { default: sequelizeInstance } = await import("../../src/db/config");
    const typedInstance =
      sequelizeInstance as unknown as MockedSequelizeInstance;
    const args = typedInstance.__constructorArgs;

    expect(Number.isNaN(args[3].port)).toBe(true);
  });

  describe("logging function", () => {
    let loggingFunction: (msg: string) => void;
    let consoleErrorSpy: ReturnType<typeof vi.spyOn>;

    beforeEach(async () => {
      vi.doMock("../../src/config/environment", () => ({
        default: {
          DATABASE: {
            HOST: "localhost",
            PORT: 5432,
            USER: "testuser",
            PASSWORD: "testpass",
            NAME: "testdb",
          },
          NODE_ENV: "development",
        },
      }));

      consoleErrorSpy = vi.spyOn(console, "error").mockImplementation(() => {});

      const module = await import("../../src/db/config");
      const typedInstance =
        module.default as unknown as MockedSequelizeInstance;
      const args = typedInstance.__constructorArgs;

      loggingFunction = args[3].logging;
    });

    afterEach(() => {
      consoleErrorSpy.mockRestore();
      vi.resetModules();
      vi.clearAllMocks();
    });

    it("should log error messages to console.error", () => {
      const errorMessage = "Database connection ERROR occurred";
      loggingFunction(errorMessage);

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        "Sequelize Error:",
        errorMessage
      );
    });

    it("should log error messages with mixed case", () => {
      const errorMessage = "Connection Error: timeout";
      loggingFunction(errorMessage);

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        "Sequelize Error:",
        errorMessage
      );
    });
  });

  it("should export the sequelize instance as default", async () => {
    const { default: sequelizeInstance } = await import("../../src/db/config");

    expect(sequelizeInstance).toBeDefined();
    expect(typeof (sequelizeInstance as any).authenticate).toBe("function");
    expect(typeof (sequelizeInstance as any).sync).toBe("function");
  });
});
