import { describe, it, expect, vi, beforeEach } from 'vitest';
import reducer, {
    fetchRoadmaps,
    addRoadmap,
    removeRoadmap,
    updateSoldCredits,
    upsertRoadmap,
} from '../../src/redux/roadmapSlice';
import type { Roadmap } from '../../src/redux/roadmapSlice';
import axios from 'axios';

vi.mock('axios');
const mockedAxios = axios as jest.Mocked<typeof axios>;

const mockRoadmap: Roadmap = {
    preId: 'pre-001',
    roadmapId: 'rm-123',
    roadmapName: 'Test Roadmap',
    roadmapDescription: 'Description',
    progress: 50,
    preAddress: 'addr1xyz',
    totalPlasticCredits: 1000,
    soldPlasticCredits: 400,
    totalPlasticTokens: 2000,
    sentPlasticTokens: 1000,
    totalPlastic: 500,
    recoveredPlastic: 100,
    createdAt: '2024-01-01',
    status: 'ACTIVE',
    fundsMissing: '0',
    fundsDistributed: '100',
};

describe('roadmapSlice reducer', () => {
    let initialState: ReturnType<typeof reducer>;

    beforeEach(() => {
        initialState = {
            roadmaps: [],
            loading: false,
            error: null,
        };
    });

    it('should handle addRoadmap', () => {
        const next = reducer(initialState, addRoadmap(mockRoadmap));
        expect(next.roadmaps).toHaveLength(1);
        expect(next.roadmaps[0].roadmapId).toBe('rm-123');
    });

    it('should handle removeRoadmap', () => {
        const withRoadmap = {
            ...initialState,
            roadmaps: [mockRoadmap],
        };
        const next = reducer(withRoadmap, removeRoadmap('rm-123'));
        expect(next.roadmaps).toHaveLength(0);
    });

    it('should handle updateSoldCredits', () => {
        const withRoadmap = {
            ...initialState,
            roadmaps: [mockRoadmap],
        };
        const next = reducer(
            withRoadmap,
            updateSoldCredits({ roadmapId: 'rm-123', soldPlasticCredit: 100 })
        );
        expect(next.roadmaps[0].soldPlasticCredits).toBe(500);
    });

    it('should handle upsertRoadmap for existing roadmap', () => {
        const updated = { ...mockRoadmap, roadmapName: 'Updated Name' };
        const stateWithRoadmap = {
            ...initialState,
            roadmaps: [mockRoadmap],
        };
        const next = reducer(stateWithRoadmap, upsertRoadmap(updated));
        expect(next.roadmaps[0].roadmapName).toBe('Updated Name');
    });

    it('should handle upsertRoadmap for new roadmap', () => {
        const newRoadmap = { ...mockRoadmap, roadmapId: 'rm-999' };
        const next = reducer(initialState, upsertRoadmap(newRoadmap));
        expect(next.roadmaps).toHaveLength(1);
        expect(next.roadmaps[0].roadmapId).toBe('rm-999');
    });

    it('should handle fetchRoadmaps.pending', () => {
        const next = reducer(initialState, {
            type: fetchRoadmaps.pending.type,
        });
        expect(next.loading).toBe(true);
        expect(next.error).toBeNull();
    });

    it('should handle fetchRoadmaps.fulfilled', () => {
        const payload = [mockRoadmap];
        const next = reducer(initialState, {
            type: fetchRoadmaps.fulfilled.type,
            payload,
        });
        expect(next.loading).toBe(false);
        expect(next.roadmaps).toEqual(payload);
    });

    it('should handle fetchRoadmaps.rejected', () => {
        const next = reducer(initialState, {
            type: fetchRoadmaps.rejected.type,
            payload: 'Error fetching roadmaps',
        });
        expect(next.loading).toBe(false);
        expect(next.error).toBe('Error fetching roadmaps');
    });
});

describe('fetchRoadmaps asyncThunk', () => {
    beforeEach(() => {
        vi.resetAllMocks();
    });

    it('should dispatch fulfilled when axios returns data', async () => {
        mockedAxios.get.mockResolvedValueOnce({
            data: { roadmaps: [mockRoadmap] },
        });

        const dispatch = vi.fn();
        const getState = vi.fn();

        const result = await fetchRoadmaps(undefined)(dispatch, getState, undefined);

        expect(dispatch).toHaveBeenCalled();
        expect(result.type).toBe('roadmaps/fetchRoadmaps/fulfilled');
        expect(result.payload).toEqual([mockRoadmap]);
    });

    it('should dispatch rejected when axios returns server error', async () => {
        mockedAxios.get.mockRejectedValueOnce({
            response: {
                data: 'Server error occurred',
            },
        });

        const dispatch = vi.fn();
        const getState = vi.fn();

        const result = await fetchRoadmaps(undefined)(dispatch, getState, undefined);

        expect(dispatch).toHaveBeenCalled();
        expect(result.type).toBe('roadmaps/fetchRoadmaps/rejected');
        expect(result.payload).toBe('Server error occurred');
    });

    it('should dispatch rejected when axios throws general error', async () => {
        mockedAxios.get.mockRejectedValueOnce(new Error('Network Error'));

        const dispatch = vi.fn();
        const getState = vi.fn();

        const result = await fetchRoadmaps(undefined)(dispatch, getState, undefined);

        expect(dispatch).toHaveBeenCalled();
        expect(result.type).toBe('roadmaps/fetchRoadmaps/rejected');
        expect(result.payload).toBe('Error fetching roadmaps');
    });
});
