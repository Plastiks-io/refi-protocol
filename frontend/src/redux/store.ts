import { configureStore } from "@reduxjs/toolkit";
import walletReducer from "./walletSlice";
import roadmapReducer from "./roadmapSlice";
import completedRoadmapReducer from "./completedRoadmapSlice";
import storageSession from "redux-persist/lib/storage/session"; // Uses sessionStorage
import { persistReducer, persistStore, PersistConfig } from "redux-persist";

// Define persist configuration with types
const persistConfig: PersistConfig<ReturnType<typeof walletReducer>> = {
  key: "wallet", // Use a more generic key
  storage: storageSession, // Store in sessionStorage
  whitelist: ["walletId", "walletAddress"], // Persist only necessary fields
};

const persistedWalletReducer = persistReducer(persistConfig, walletReducer);

export const store = configureStore({
  reducer: {
    wallet: persistedWalletReducer,
    roadmaps: roadmapReducer,
    completedRoadmaps: completedRoadmapReducer,
  },
  middleware: (getDefaultMiddleware) =>
    getDefaultMiddleware({
      serializableCheck: false, // Disable serializability check for redux-persist
    }),
});

export type RootState = ReturnType<typeof store.getState>;
export type AppDispatch = typeof store.dispatch;

export const persistor = persistStore(store);
