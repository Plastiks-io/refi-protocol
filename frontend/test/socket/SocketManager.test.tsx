import React from "react";
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render } from "@testing-library/react";
import SocketManager from "../../src/socket/SocketManager";
import * as CardanoContext from "../../src/contexts/cardanoContexts";
import * as RoadmapSlice from "../../src/redux/roadmapSlice";
import * as TransactionSlice from "../../src/redux/TransactionSlice";
import { socket } from "../../src/socket/socket";

const mockDispatch = vi.fn();

vi.mock("react-redux", async () => {
  const actual = await vi.importActual<typeof import("react-redux")>(
    "react-redux"
  );
  return {
    ...actual,
    useDispatch: () => mockDispatch,
  };
});

vi.mock("../../src/socket/socket", () => ({
  socket: {
    on: vi.fn(),
    off: vi.fn(),
  },
}));

vi.spyOn(CardanoContext, "useCardanoData").mockReturnValue({
  refresh: vi.fn().mockResolvedValue(undefined),
  data: null,
});

describe("SocketManager Component", () => {
  beforeEach(() => {
    vi.clearAllMocks();

    vi.spyOn(RoadmapSlice, "upsertRoadmap").mockImplementation(
      (data: any) => data
    );
    vi.spyOn(TransactionSlice, "fetchTransactions").mockImplementation(
      (args: any) => args
    );
  });

  it("should subscribe to socket events on mount", () => {
    render(<SocketManager />);

    expect(socket.on).toHaveBeenCalledWith(
      "roadmapUpdated",
      expect.any(Function)
    );
    expect(socket.on).toHaveBeenCalledWith(
      "stakeContractUpdated",
      expect.any(Function)
    );
  });

  it("should dispatch upsertRoadmap when roadmapUpdated is received", () => {
    render(<SocketManager />);

    const handler = (socket.on as any).mock.calls.find(
      ([eventName]: any[]) => eventName === "roadmapUpdated"
    )[1];

    const mockRoadmap = { id: "123", title: "Test Roadmap" };
    handler(mockRoadmap);

    expect(mockDispatch).toHaveBeenCalledWith(mockRoadmap);
  });

  it("should do nothing when stakeContractUpdated is false", async () => {
    render(<SocketManager />);

    const handler = (socket.on as any).mock.calls.find(
      ([eventName]: any[]) => eventName === "stakeContractUpdated"
    )[1];

    await handler(false);

    expect(mockDispatch).not.toHaveBeenCalled();
  });

  it("should unsubscribe from socket events on unmount", () => {
    const { unmount } = render(<SocketManager />);
    unmount();

    expect(socket.off).toHaveBeenCalledWith("roadmapUpdated");
    expect(socket.off).toHaveBeenCalledWith("stakeContractUpdated");
  });
});
