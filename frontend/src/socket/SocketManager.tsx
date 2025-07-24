import { useCardanoData } from "@/contexts/cardanoContexts";
import { AppDispatch } from "@/redux/store";
import React, { useEffect } from "react";
import { useDispatch } from "react-redux";
import { socket } from "./socket";
import { Roadmap, upsertRoadmap } from "@/redux/roadmapSlice";
import { fetchTransactions, TransactionType } from "@/redux/TransactionSlice";

const SocketManager: React.FC = () => {
  const dispatch = useDispatch<AppDispatch>();
  const { refresh } = useCardanoData(); // Now this is inside CardanoProvider

  useEffect(() => {
    // ✅ Socket.IO: listen for roadmap updates
    socket.on("roadmapUpdated", (fullRoadmap: Roadmap) => {
      dispatch(upsertRoadmap(fullRoadmap));
    });

    // ✅ Socket.IO: listen for stake Contract Update
    socket.on("stakeContractUpdated", (isUpdated: boolean) => {
      console.log("stakeContractUpdated", isUpdated);
      if (isUpdated) {
        refresh().catch(console.error);
        dispatch(
          fetchTransactions({
            page: 1,
            perPage: 10,
            type: [TransactionType.Sold, TransactionType.Transfer],
          })
        );
      }
    });

    return () => {
      socket.off("roadmapUpdated");
      socket.off("stakeContractUpdated");
    };
  }, [dispatch, refresh]);

  return null; // This component doesn't render anything
};

export default SocketManager;
