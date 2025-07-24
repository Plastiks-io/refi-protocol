// test/contract/contracts.test.ts
import { describe, it, expect, beforeEach, afterEach, vi } from "vitest";
import * as Validator from "../../src/contract/contracts";

describe("Lucid Validator exports", () => {
  const originalEnv = process.env;

  beforeEach(() => {
    // 환경 변수 백업 후 설정
    process.env = {
      ...originalEnv,
      STAKE_REWARD_CBOR: "4e4d01000033222220051200120011", // 예시 CBOR
      REFI_CBOR: "4e4d010000998877665544332211", // 예시 CBOR
    };

    // 모듈 캐시 초기화 (환경 변수 반영 위해)
    vi.resetModules();
  });

  afterEach(() => {
    process.env = originalEnv;
  });

  // stakeRewardValidator 내보내기 확인: 올바른 형식과 스크립트
  it("should export stakeRewardValidator with type and script", async () => {
    const { stakeRewardValidator } = await import(
      "../../src/contract/contracts"
    );

    expect(stakeRewardValidator).toBeDefined();
    expect(stakeRewardValidator.type).toBe("PlutusV2");
    expect(stakeRewardValidator.script).toBe("4e4d01000033222220051200120011");
  });

  // refiValidator 내보내기 확인: 올바른 형식과 스크립트
  it("should export refiValidator with type and script", async () => {
    const { refiValidator } = await import("../../src/contract/contracts");

    expect(refiValidator).toBeDefined();
    expect(refiValidator.type).toBe("PlutusV2");
    expect(refiValidator.script).toBe("4e4d010000998877665544332211");
  });
});
