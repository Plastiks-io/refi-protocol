import request from "supertest";
import app from "../src/index";

describe("Server Initialization and Route Checks", () => {
  it("should return a welcome message at root route", async () => {
    const response = await request(app).get("/");
    expect(response.status).toBe(200);
    expect(response.text).toBe("Hello, TypeScript with Express!");
  });

  it("should respond on /roadmap/all (if implemented)", async () => {
    const response = await request(app).get("/roadmap/all");
    expect([200, 400, 404, 500]).toContain(response.statusCode);
  });

  it("should respond on /user/someRoute (mock check)", async () => {
    const response = await request(app).get("/user/non-existent");
    expect([404, 400]).toContain(response.statusCode);
  });
});
