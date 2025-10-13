// src/socket.ts
import { io, type Socket } from "socket.io-client";

const getSocketConfig = () => {
  // Check if we're in development mode
  const isDevelopment =
    import.meta.env.DEV || import.meta.env.MODE === "development";

  if (isDevelopment) {
    // Development configuration
    return {
      url: import.meta.env.VITE_SERVER_URL,
      options: {
        withCredentials: true,
      },
    };
  } else {
    // Production/deployment configuration
    return {
      url: window.location.origin,
      options: {
        withCredentials: true,
        path: "/api/socket.io",
      },
    };
  }
};

const { url, options } = getSocketConfig();
export const socket: Socket = io(url, options);

// // src/socket.ts
// import { io, type Socket } from "socket.io-client";

// // For deployment
// // export const socket: Socket = io(window.location.origin, {
// //   withCredentials: true,
// //   path: "/api/socket.io",
// // });

// // For Devlopment
// export const socket: Socket = io(import.meta.env.VITE_SERVER_URL, {
//   withCredentials: true,
// });
