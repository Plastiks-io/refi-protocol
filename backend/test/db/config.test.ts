import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { Sequelize } from "sequelize";

// Mock dependencies
vi.mock("sequelize");
vi.mock("dotenv");

describe("Sequelize Configuration", () => {
  let mockSequelizeInstance;
  let mockDotenvConfig;
  let originalEnv;

  beforeEach(() => {
    // Store original env
    originalEnv = process.env;

    // Create mock Sequelize instance
    mockSequelizeInstance = {
      authenticate: vi.fn(),
      close: vi.fn(),
      sync: vi.fn(),
    };

    // Mock Sequelize constructor
    vi.mocked(Sequelize).mockImplementation(() => mockSequelizeInstance);

    // Mock dotenv
    mockDotenvConfig = vi.fn();
    vi.doMock("dotenv", () => ({
      default: { config: mockDotenvConfig },
      config: mockDotenvConfig,
    }));

    // Clear module cache to ensure fresh import
    vi.resetModules();
  });

  afterEach(() => {
    // Restore original env
    process.env = originalEnv;
    vi.clearAllMocks();
  });

  it("should call dotenv.config() on module import", async () => {
    // Import the module
    await import("../../src/db/config");

    expect(mockDotenvConfig).toHaveBeenCalledOnce();
  });

  it("should create Sequelize instance with correct configuration", async () => {
    // Set up environment variables
    process.env.DB_HOST = "localhost";
    process.env.DB_PORT = "5432";
    process.env.DB_USER = "testuser";
    process.env.DB_PASSWORD = "testpass";
    process.env.DB_NAME = "testdb";

    // Import the module
    await import("../../src/db/config");

    expect(Sequelize).toHaveBeenCalledWith({
      host: "localhost",
      port: 5432,
      dialect: "postgres",
      username: "testuser",
      password: "testpass",
      database: "testdb",
      logging: expect.any(Function),
    });
  });

  it("should use default port 5432 when DB_PORT is not set", async () => {
    // Set up environment variables without DB_PORT
    process.env.DB_HOST = "localhost";
    process.env.DB_USER = "testuser";
    process.env.DB_PASSWORD = "testpass";
    process.env.DB_NAME = "testdb";
    delete process.env.DB_PORT;

    // Import the module
    await import("../../src/db/config");

    expect(Sequelize).toHaveBeenCalledWith(
      expect.objectContaining({
        port: 5432,
      })
    );
  });

  it("should parse custom DB_PORT correctly", async () => {
    // Set up environment variables with custom port
    process.env.DB_HOST = "localhost";
    process.env.DB_PORT = "3306";
    process.env.DB_USER = "testuser";
    process.env.DB_PASSWORD = "testpass";
    process.env.DB_NAME = "testdb";

    // Import the module
    await import("../../src/db/config");

    expect(Sequelize).toHaveBeenCalledWith(
      expect.objectContaining({
        port: 3306,
      })
    );
  });

  it("should handle invalid DB_PORT gracefully", async () => {
    // Set up environment variables with invalid port
    process.env.DB_HOST = "localhost";
    process.env.DB_PORT = "invalid";
    process.env.DB_USER = "testuser";
    process.env.DB_PASSWORD = "testpass";
    process.env.DB_NAME = "testdb";

    // Import the module
    await import("../../src/db/config");

    expect(Sequelize).toHaveBeenCalledWith(
      expect.objectContaining({
        port: NaN, // parseInt of 'invalid' returns NaN
      })
    );
  });

  describe("logging function", () => {
    let loggingFunction;
    let consoleSpy;

    beforeEach(async () => {
      // Set up environment variables
      process.env.DB_HOST = "localhost";
      process.env.DB_USER = "testuser";
      process.env.DB_PASSWORD = "testpass";
      process.env.DB_NAME = "testdb";

      // Spy on console.error
      consoleSpy = vi.spyOn(console, "error").mockImplementation(() => {});

      // Import the module and extract logging function
      await import("../../src/db/config");

      const sequelizeCallArgs = vi.mocked(Sequelize).mock.calls[0];
      const options = sequelizeCallArgs[1] || sequelizeCallArgs[0];
      // Ensure options is an object before accessing logging
      if (typeof options === "object" && "logging" in options) {
        loggingFunction = (options as { logging: (msg: string) => void })
          .logging;
      } else {
        throw new Error(
          "Sequelize options does not contain a logging function"
        );
      }
    });

    afterEach(() => {
      consoleSpy.mockRestore();
    });

    it("should log error messages to console.error", () => {
      const errorMessage = "Database connection ERROR occurred";

      loggingFunction(errorMessage);

      expect(consoleSpy).toHaveBeenCalledWith("Sequelize Error:", errorMessage);
    });

    it("should log error messages with mixed case", () => {
      const errorMessage = "Connection Error: timeout";

      loggingFunction(errorMessage);

      expect(consoleSpy).toHaveBeenCalledWith("Sequelize Error:", errorMessage);
    });
  });

  it("should export the sequelize instance as default", async () => {
    // Set up environment variables
    process.env.DB_HOST = "localhost";
    process.env.DB_USER = "testuser";
    process.env.DB_PASSWORD = "testpass";
    process.env.DB_NAME = "testdb";

    // Import the module
    const sequelizeModule = await import("../../src/db/config");

    expect(sequelizeModule.default).toBe(mockSequelizeInstance);
  });
});
