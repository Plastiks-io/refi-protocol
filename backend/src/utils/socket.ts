// src/utils/socket.ts
import { Server as IOServer } from "socket.io";
let io: IOServer;

export function initIO(serverIO: IOServer) {
  io = serverIO;
}
export function getIO(): IOServer {
  if (!io) throw new Error("Socket.IO not initialized");
  return io;
}
