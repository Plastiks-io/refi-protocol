// src/socket.ts
import { io, Socket } from "socket.io-client";

export const socket: Socket = io(import.meta.env.VITE_SERVER_URL, {
  withCredentials: true,
  path: "/socket.io",
});
