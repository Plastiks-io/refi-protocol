// src/socket.ts
import { io, type Socket } from "socket.io-client";

// ✅ Correct: Base URL without /api
const BACKEND_URL =
  import.meta.env.VITE_SERVER_URL || "http://localhost:8080/api";

// Remove trailing /api if present
const BACKEND = BACKEND_URL.replace(/\/api\/?$/, "");

// ✅ Correct: Full path including /api
const OPTIONS = {
  withCredentials: true,
  path: "/api/socket.io", // This matches your backend configuration
};

export const socket: Socket = io(BACKEND, OPTIONS);

// Dev debug
socket.on("connect", () => console.log("[socket] connected:", socket.id));
socket.on("connect_error", (err) =>
  console.error("[socket] connect_error:", err)
);
socket.on("disconnect", (reason) =>
  console.log("[socket] disconnected:", reason)
);
