// __tests__/adminAuth.test.ts
import { describe, it, expect, vi, beforeEach } from "vitest";
import { adminAuth } from "../../src/middleware/adminAuth";
import jwt from "jsonwebtoken";
import { Role } from "../../src/models/admin.model";

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
    delete process.env.JWT_SECRET;
    delete process.env.JWT_COOKIE_NAME;
    mockNext.mockClear();
  });

  it("should respond 401 if token is missing in cookies", async () => {
    const req = mockReq();
    const res = mockRes();

    process.env.JWT_SECRET = "secret";

    await adminAuth(req, res, mockNext);

    expect(res.status).toHaveBeenCalledWith(401);
    expect(res.json).toHaveBeenCalledWith({
      message: "Missing authentication token in cookies",
    });
    expect(mockNext).not.toHaveBeenCalled();
  });

  it("should respond 500 if JWT_SECRET is missing", async () => {
    const req = mockReq();
    req.cookies.token = "sometoken";
    const res = mockRes();

    await adminAuth(req, res, mockNext);

    expect(res.status).toHaveBeenCalledWith(500);
    expect(res.json).toHaveBeenCalledWith({
      message: "Server configuration error",
    });
    expect(mockNext).not.toHaveBeenCalled();
  });

  it("should respond 401 if token is invalid", async () => {
    const req = mockReq();
    req.cookies.token = "invalidtoken";
    const res = mockRes();

    process.env.JWT_SECRET = "secret";
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
    const req = mockReq();
    req.cookies.token = "validtoken";
    const res = mockRes();

    process.env.JWT_SECRET = "secret";
    vi.spyOn(jwt, "verify").mockReturnValue({
      id: "alien123",
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
    const req = mockReq();
    req.cookies.token = "validtoken";
    const res = mockRes();

    process.env.JWT_SECRET = "secret";
    vi.spyOn(jwt, "verify").mockReturnValue({ ...VALID_TOKEN_PAYLOAD } as any);

    await adminAuth(req, res, mockNext);

    expect(req.admin).toEqual(VALID_TOKEN_PAYLOAD);
    expect(mockNext).toHaveBeenCalled();
  });

  it("should call next() and attach admin if role is REGULAR", async () => {
    const req = mockReq();
    req.cookies.token = "validtoken";
    const res = mockRes();

    process.env.JWT_SECRET = "secret";
    vi.spyOn(jwt, "verify").mockReturnValue({
      ...VALID_TOKEN_PAYLOAD,
      role: Role.REGULAR,
    } as any);

    await adminAuth(req, res, mockNext);

    expect(req.admin).toMatchObject({ role: Role.REGULAR });
    expect(mockNext).toHaveBeenCalled();
  });

  it("should read token from custom cookie name", async () => {
    const req = mockReq();
    req.cookies.admin_token = "validtoken";
    const res = mockRes();

    process.env.JWT_SECRET = "secret";
    process.env.JWT_COOKIE_NAME = "admin_token";

    vi.spyOn(jwt, "verify").mockReturnValue({
      ...VALID_TOKEN_PAYLOAD,
    } as any);

    await adminAuth(req, res, mockNext);

    expect(req.admin).toEqual(VALID_TOKEN_PAYLOAD);
    expect(mockNext).toHaveBeenCalled();
  });
});
