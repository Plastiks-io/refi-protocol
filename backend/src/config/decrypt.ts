// src/utils/decrypt-refi.ts - KEEP AS IS
import crypto from "crypto";

const ALGORITHM = "aes-256-gcm";

export function decryptRefi(encryptedText: string): string {
  if (!encryptedText) {
    throw new Error("Encrypted text is required");
  }

  if (!encryptedText.includes(":")) {
    console.warn("⚠️  Using unencrypted credential - consider encrypting it");
    return encryptedText;
  }

  const encryptionKey = process.env.REFI_ENC;

  if (!encryptionKey) {
    throw new Error(
      "REFI_ENC (encryption key) not found in environment variables"
    );
  }

  try {
    const parts = encryptedText.split(":");

    if (parts.length !== 3) {
      throw new Error(
        "Invalid encrypted text format. Expected format: iv:authTag:encrypted"
      );
    }

    const iv = Buffer.from(parts[0], "hex");
    const authTag = Buffer.from(parts[1], "hex");
    const encrypted = Buffer.from(parts[2], "hex");

    const decipher = crypto.createDecipheriv(
      ALGORITHM,
      Buffer.from(encryptionKey, "hex"),
      iv
    );

    decipher.setAuthTag(authTag);

    let decrypted = decipher.update(encrypted);
    decrypted = Buffer.concat([decrypted, decipher.final()]);

    return decrypted.toString("utf8");
  } catch (error: any) {
    throw new Error(`Decryption failed: ${error.message}`);
  }
}

export function isEncryptedRefi(text: string | undefined): boolean {
  if (!text) return false;
  return text.includes(":") && text.split(":").length === 3;
}

export function safeDecryptRefi(value: string | undefined): string {
  if (!value) return "";

  try {
    return isEncryptedRefi(value) ? decryptRefi(value) : value;
  } catch (error) {
    console.error("Failed to decrypt value:", error);
    throw error;
  }
}
