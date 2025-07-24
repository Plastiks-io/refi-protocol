import { describe, it, expect, beforeEach } from 'vitest';
import reducer, { setWallet, disconnectWallet } from '../../src/redux/walletSlice';

describe('walletSlice reducer', () => {
    let initialState: ReturnType<typeof reducer>;

    beforeEach(() => {
        initialState = {
            walletId: null,
            walletAddress: null,
        };
    });

    it('should handle setWallet correctly', () => {
        const next = reducer(
            initialState,
            setWallet({ walletId: 'wallet-123', address: 'addr_test1xyz' })
        );
        expect(next.walletId).toBe('wallet-123');
        expect(next.walletAddress).toBe('addr_test1xyz');
    });

    it('should handle disconnectWallet correctly', () => {
        const preConnectedState = {
            walletId: 'wallet-abc',
            walletAddress: 'addr_test123',
        };
        const next = reducer(preConnectedState, disconnectWallet());
        expect(next.walletId).toBeNull();
        expect(next.walletAddress).toBeNull();
    });

    it('should return initial state on unknown action', () => {
        const next = reducer(initialState, { type: 'unknown' } as any);
        expect(next).toEqual(initialState);
    });
});
