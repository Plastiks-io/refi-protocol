import { describe, it, expect, beforeEach } from "vitest";
import reducer, { setAuthUser, clearAuthUser } from "../../src/redux/authSlice";

describe("authSlice", () => {
  let initialState: ReturnType<typeof reducer>;

  beforeEach(() => {
    initialState = {
      role: null,
      isAuthenticated: false,
    };
  });

  it("should return the initial state by default", () => {
    const state = reducer(undefined, { type: "USER" });
    expect(state).toEqual({
      role: null,
      isAuthenticated: false,
    });
  });

  it("should handle setAuthUser for SUPER_ADMIN", () => {
    const action = setAuthUser({ role: "SUPER_ADMIN" });
    const state = reducer(initialState, action);
    expect(state.role).toBe("SUPER_ADMIN");
    expect(state.isAuthenticated).toBe(true);
  });

  it("should handle setAuthUser for ADMIN", () => {
    const action = setAuthUser({ role: "ADMIN" });
    const state = reducer(initialState, action);
    expect(state.role).toBe("ADMIN");
    expect(state.isAuthenticated).toBe(true);
  });

  it("should handle setAuthUser for USER", () => {
    const action = setAuthUser({ role: "USER" });
    const state = reducer(initialState, action);
    expect(state.role).toBe("USER");
    expect(state.isAuthenticated).toBe(true);
  });

  it("should handle clearAuthUser", () => {
    const loggedInState = {
      role: "ADMIN" as "SUPER_ADMIN" | "ADMIN" | "USER",
      isAuthenticated: true,
    };
    const state = reducer(loggedInState, clearAuthUser());
    expect(state.role).toBe(null);
    expect(state.isAuthenticated).toBe(false);
  });
});
