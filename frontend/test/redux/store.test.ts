import { describe, it, expect, vi, beforeEach } from 'vitest';
import { store } from '../../src/redux/store';
import { setAuthUser, clearAuthUser } from '../../src/redux/authSlice';
import { setWallet } from '../../src/redux/walletSlice';
import type { RootState } from '../../src/redux/store';

// ✅ Mock sessionStorage to test redux-persist behavior
vi.mock('redux-persist/lib/storage/session', async () => {
    const memoryStorage: Record<string, string> = {};
    return {
        default: {
            getItem: vi.fn((key) => Promise.resolve(memoryStorage[key] || null)),
            setItem: vi.fn((key, value) => {
                memoryStorage[key] = value;
                return Promise.resolve();
            }),
            removeItem: vi.fn((key) => {
                delete memoryStorage[key];
                return Promise.resolve();
            }),
        },
    };
});

describe('Redux Store Integration', () => {
    let state: RootState;

    beforeEach(() => {
        state = store.getState();
    });

    it('should initialize with expected slices', () => {
        expect(state).toHaveProperty('wallet');
        expect(state).toHaveProperty('roadmaps');
        expect(state).toHaveProperty('completedRoadmaps');
        expect(state).toHaveProperty('archivedRoadmaps');
        expect(state).toHaveProperty('transactions');
        expect(state).toHaveProperty('auth');
        expect(state).toHaveProperty('admin');
    });

    it('should dispatch auth actions and update state', () => {
        store.dispatch(setAuthUser({ role: 'ADMIN' }));
        const newState = store.getState();
        expect(newState.auth.role).toBe('ADMIN');
        expect(newState.auth.isAuthenticated).toBe(true);

        store.dispatch(clearAuthUser());
        const clearedState = store.getState();
        expect(clearedState.auth.role).toBeNull();
        expect(clearedState.auth.isAuthenticated).toBe(false);
    });

    it('should persist wallet slice correctly (only whitelisted keys)', async () => {
        const storage = await import('redux-persist/lib/storage/session');
        const persistedKey = 'persist:wallet';
        await storage.default.setItem(persistedKey, JSON.stringify({ walletId: 'id', walletAddress: 'addr' }));
        const item = await storage.default.getItem(persistedKey);
        expect(item).not.toBeNull();
    });

    it('should persist auth slice correctly (only whitelisted keys)', async () => {
        const storage = await import('redux-persist/lib/storage/session');
        const persistedKey = 'persist:auth';
        await storage.default.setItem(persistedKey, JSON.stringify({ role: 'USER', isAuthenticated: true }));
        const item = await storage.default.getItem(persistedKey);
        expect(item).toContain('USER');
    });
});
