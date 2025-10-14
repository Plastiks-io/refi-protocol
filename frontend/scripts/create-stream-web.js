// scripts/create-stream-web.js (ES Module version)
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

// ES module equivalent of __dirname
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const targetDir = path.join(__dirname, "../node_modules/stream-browserify");
const targetFile = path.join(targetDir, "web.js");
const content = "export * from 'web-streams-polyfill';\n";

try {
  // Create directory if it doesn't exist
  if (!fs.existsSync(targetDir)) {
    fs.mkdirSync(targetDir, { recursive: true });
    console.log("✅ Created stream-browserify directory");
  }

  // Write the web.js file
  fs.writeFileSync(targetFile, content);
  console.log("✅ Created stream-browserify/web.js file");
  console.log("📝 Content:", content.trim());
} catch (error) {
  console.warn(
    "⚠️  Warning: Could not create stream-browserify/web.js:",
    error.message
  );
  // Don't fail the build process
  process.exit(0);
}
