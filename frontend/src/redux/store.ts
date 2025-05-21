import { configureStore } from "@reduxjs/toolkit";
import walletReducer from "./walletSlice";
import roadmapReducer from "./roadmapSlice";
import completedRoadmapReducer from "./completedRoadmapSlice";
import archivedRoadmapReducer from "./archivedRoadmapSlice";
import TransactionsReducer from "./TransactionSlice";
import AuthReducer from "./authSlice";
import AdminReducer from "./adminSlice";
import storageSession from "redux-persist/lib/storage/session";
import { persistReducer, persistStore, PersistConfig } from "redux-persist";

// Define persist configuration with types
const persistConfig: PersistConfig<ReturnType<typeof walletReducer>> = {
  key: "wallet", // Use a more generic key
  storage: storageSession, // Store in sessionStorage
  whitelist: ["walletId", "walletAddress"], // Persist only necessary fields
};

// Persist config for auth
const authPersistConfig: PersistConfig<ReturnType<typeof AuthReducer>> = {
  key: "auth",
  storage: storageSession,
  whitelist: ["email", "role", "isAuthenticated"],
};

const persistedWalletReducer = persistReducer(persistConfig, walletReducer);
const persistedAuthReducer = persistReducer(authPersistConfig, AuthReducer);

export const store = configureStore({
  reducer: {
    wallet: persistedWalletReducer,
    roadmaps: roadmapReducer,
    completedRoadmaps: completedRoadmapReducer,
    archivedRoadmaps: archivedRoadmapReducer,
    transactions: TransactionsReducer,
    auth: persistedAuthReducer,
    admin: AdminReducer,
  },
  middleware: (getDefaultMiddleware) =>
    getDefaultMiddleware({
      serializableCheck: false, // Disable serializability check for redux-persist
    }),
});

export type RootState = ReturnType<typeof store.getState>;
export type AppDispatch = typeof store.dispatch;

export const persistor = persistStore(store);
