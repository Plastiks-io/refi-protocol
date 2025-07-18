// src/socket.ts
import { io, Socket } from "socket.io-client";

export const socket: Socket = io(
  process.env.VITE_SERVER_URL || "http://localhost:8080",
  {
    withCredentials: true,
  }
);
