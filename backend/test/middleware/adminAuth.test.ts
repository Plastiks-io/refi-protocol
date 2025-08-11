import { describe, it, expect, vi, beforeEach } from "vitest";
import jwt from "jsonwebtoken";
import { Role } from "../../src/models/admin.model";

const defaultConfigMock = {
  JWT_SECRET: "secret",
  JWT_COOKIE_NAME: "token",
};

async function importWithConfigMock(configOverrides = {}) {
  vi.doMock("../../src/config/environment", () => ({
    default: {
      ...defaultConfigMock,
      ...configOverrides,
    },
  }));
  return (await import("../../src/middleware/adminAuth")).adminAuth;
}

describe("adminAuth middleware", () => {
  const mockReq = () => ({ cookies: {} } as any);
  const mockRes = () => {
    const res: any = {};
    res.status = vi.fn().mockReturnValue(res);
    res.json = vi.fn().mockReturnValue(res);
    return res;
  };
  const mockNext = vi.fn();

  const VALID_TOKEN_PAYLOAD = {
    id: "admin123",
    email: "admin@example.com",
    role: Role.SUPER,
  };

  beforeEach(() => {
    vi.restoreAllMocks();
    mockNext.mockClear();
  });

  it("should respond 401 if token is missing in cookies", async () => {
    const adminAuth = await importWithConfigMock();
    const req = mockReq();
    const res = mockRes();

    await adminAuth(req, res, mockNext);

    expect(res.status).toHaveBeenCalledWith(401);
    expect(res.json).toHaveBeenCalledWith({
      message: "Missing authentication token in cookies",
    });
    expect(mockNext).not.toHaveBeenCalled();
  });

  it("should respond 401 if JWT_SECRET is missing", async () => {
    const adminAuthMissingSecret = await importWithConfigMock({
      JWT_SECRET: undefined,
    });
    const req = mockReq();
    req.cookies.token = "someinvalidtoken";
    const res = mockRes();

    await adminAuthMissingSecret(req, res, mockNext);

    expect(res.status).toHaveBeenCalledWith(401);
    expect(res.json).toHaveBeenCalledWith({
      message: "Invalid or expired authentication token",
    });
    expect(mockNext).not.toHaveBeenCalled();
  });

  it("should respond 401 if token is invalid", async () => {
    const adminAuth = await importWithConfigMock();
    const req = mockReq();
    req.cookies.token = "invalidtoken";
    const res = mockRes();

    vi.spyOn(jwt, "verify").mockImplementation(() => {
      throw new Error("invalid token");
    });

    await adminAuth(req, res, mockNext);

    expect(res.status).toHaveBeenCalledWith(401);
    expect(res.json).toHaveBeenCalledWith({
      message: "Invalid or expired authentication token",
    });
    expect(mockNext).not.toHaveBeenCalled();
  });

  it("should respond 403 if role is not SUPER or REGULAR", async () => {
    const adminAuth = await importWithConfigMock();
    const req = mockReq();
    req.cookies.token = "validtoken";
    const res = mockRes();

    vi.spyOn(jwt, "verify").mockReturnValue({
      id: "hacker",
      email: "badguy@example.com",
      role: "ALIEN",
    } as any);

    await adminAuth(req, res, mockNext);

    expect(res.status).toHaveBeenCalledWith(403);
    expect(res.json).toHaveBeenCalledWith({
      message: "Forbidden: requires SUPER_ADMIN or ADMIN role",
    });
    expect(mockNext).not.toHaveBeenCalled();
  });

  it("should call next() and attach admin if role is SUPER", async () => {
    const adminAuth = await importWithConfigMock();
    const req = mockReq();
    req.cookies.token = "validtoken";
    const res = mockRes();

    vi.spyOn(jwt, "verify").mockReturnValue({ ...VALID_TOKEN_PAYLOAD } as any);

    await adminAuth(req, res, mockNext);

    expect(req.admin).toEqual(VALID_TOKEN_PAYLOAD);
    expect(mockNext).toHaveBeenCalled();
  });

  it("should call next() and attach admin if role is REGULAR", async () => {
    const adminAuth = await importWithConfigMock();
    const req = mockReq();
    req.cookies.token = "validtoken";
    const res = mockRes();

    vi.spyOn(jwt, "verify").mockReturnValue({
      ...VALID_TOKEN_PAYLOAD,
      role: Role.REGULAR,
    } as any);

    await adminAuth(req, res, mockNext);

    expect(req.admin).toMatchObject({ role: Role.REGULAR });
    expect(mockNext).toHaveBeenCalled();
  });

  it("should fail to read custom cookie name because middleware ignores config", async () => {
    const adminAuth = await importWithConfigMock({
      JWT_COOKIE_NAME: "admin_token",
    });
    const req = mockReq();
    req.cookies.admin_token = "validtoken";
    const res = mockRes();

    vi.spyOn(jwt, "verify").mockReturnValue(VALID_TOKEN_PAYLOAD as any);

    await adminAuth(req, res, mockNext);

    expect(req.admin).toBeUndefined();
    expect(res.status).toHaveBeenCalledWith(401);
    expect(res.json).toHaveBeenCalledWith({
      message: "Missing authentication token in cookies",
    });
    expect(mockNext).not.toHaveBeenCalled();
  });
});
