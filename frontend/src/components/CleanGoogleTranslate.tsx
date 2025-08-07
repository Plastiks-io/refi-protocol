// src/components/CleanGoogleTranslate.tsx
import React, { useEffect, useState } from "react";
import { DownArrow } from "@/assets/icons";

declare global {
  interface Window {
    google: any;
    googleTranslateElementInit: () => void;
  }
}

interface Language {
  code: string;
  name: string;
  flag: string;
}

const languages: Language[] = [
  { code: "en", name: "English", flag: "🇺🇸" },
  { code: "es", name: "Español", flag: "🇪🇸" },
  { code: "fr", name: "Français", flag: "🇫🇷" },
  { code: "de", name: "Deutsch", flag: "🇩🇪" },
  { code: "hi", name: "हिंदी", flag: "🇮🇳" },
  { code: "hy", name: "Հայերեն", flag: "🇦🇲" },
  { code: "zh-CN", name: "中文", flag: "🇨🇳" },
  { code: "ja", name: "日本語", flag: "🇯🇵" },
  { code: "ar", name: "العربية", flag: "🇸🇦" },
  { code: "pt", name: "Português", flag: "🇧🇷" },
  { code: "ru", name: "Русский", flag: "🇷🇺" },
];

const CleanGoogleTranslate: React.FC = () => {
  const [currentLang, setCurrentLang] = useState("en");

  useEffect(() => {
    // 1) Hide all Google Translate UI elements
    const style = document.createElement("style");
    style.textContent = `
      .goog-te-banner-frame,
      .goog-te-banner-frame.skiptranslate,
      #google_translate_element .goog-te-gadget,
      .goog-te-gadget,
      .goog-te-gadget-simple,
      .goog-te-gadget-icon,
      .goog-te-combo,
      div#google_translate_element,
      div#google_translate_element > div,
      div#google_translate_element > div > div {
        display: none !important;
      }
      body { top: 0 !important; margin: 0 !important; padding: 0 !important; }
    `;
    document.head.appendChild(style);

    // 2) Load & initialize the Google Translate widget
    window.googleTranslateElementInit = () => {
      new window.google.translate.TranslateElement(
        {
          pageLanguage: "en",
          includedLanguages: languages.map((l) => l.code).join(","),
          layout: window.google.translate.TranslateElement.InlineLayout.SIMPLE,
          autoDisplay: false,
        },
        "google_translate_element"
      );

      // Remove banner iframe after initialization
      setTimeout(() => {
        const banner = document.querySelector(".goog-te-banner-frame");
        if (banner) banner.remove();
        document.body.style.top = "0";
      }, 500);
    };

    if (!document.querySelector('script[src*="translate.google.com"]')) {
      const script = document.createElement("script");
      script.src =
        "//translate.google.com/translate_a/element.js?cb=googleTranslateElementInit";
      script.async = true;
      document.body.appendChild(script);
    } else if (window.google && window.google.translate) {
      window.googleTranslateElementInit();
    }

    // 3) Read current language from googtrans cookie and set it
    const getLangFromCookie = () => {
      const match = document.cookie.match(/(?:^|;\s*)googtrans=\/en\/([^;]+)/);
      return match ? match[1] : "en";
    };
    setCurrentLang(getLangFromCookie());
  }, []);

  // 4) Effect to update the hidden widget's select element on language change
  useEffect(() => {
    // Delay to ensure widget is loaded
    const timeout = setTimeout(() => {
      const select = document.querySelector<HTMLSelectElement>(
        "#google_translate_element select.goog-te-combo"
      );
      if (select) {
        // Empty string means English (default)
        select.value = currentLang === "en" ? "" : currentLang;
        select.dispatchEvent(new Event("change"));
      }
    }, 1000);

    return () => clearTimeout(timeout);
  }, [currentLang]);

  // Language change handler: updates cookie and reloads page
  const changeLanguage = (lang: string) => {
    const expires = new Date();
    expires.setDate(expires.getDate() + 365);
    const domain = window.location.hostname;
    const rootDomain = domain.startsWith("www.") ? domain.substring(4) : domain;

    // Clear all variants
    document.cookie = `googtrans=;expires=Thu, 01 Jan 1970 00:00:00 UTC;path=/`;
    document.cookie = `googtrans=;expires=Thu, 01 Jan 1970 00:00:00 UTC;path=/;domain=${domain}`;
    document.cookie = `googtrans=;expires=Thu, 01 Jan 1970 00:00:00 UTC;path=/;domain=.${rootDomain}`;

    // Set new cookie with root domain
    if (lang !== "en") {
      document.cookie = `googtrans=/en/${lang};expires=${expires.toUTCString()};path=/;domain=.${rootDomain}`;
    }

    setCurrentLang(lang);
    window.location.reload();
  };

  return (
    <div className="flex items-center space-x-3">
      {/* Hidden translate element */}
      <div
        id="google_translate_element"
        style={{
          display: "none",
          visibility: "hidden",
          height: 0,
          width: 0,
          overflow: "hidden",
        }}
      />

      {/* Language selector with down arrow */}
      <div className="relative">
        <select
          value={currentLang}
          onChange={(e) => changeLanguage(e.target.value)}
          className="appearance-none bg-white border border-gray-300 rounded-lg px-4 py-2 pr-8 text-sm text-black focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500 cursor-pointer shadow-sm hover:shadow-md transition-all"
          style={{ color: "black" }}
        >
          {languages.map((language) => (
            <option key={language.code} value={language.code}>
              {language.flag} {language.name}
            </option>
          ))}
        </select>
        <img
          src={DownArrow}
          alt="Down Arrow"
          className="pointer-events-none absolute right-2 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-500"
        />
      </div>
    </div>
  );
};

export default CleanGoogleTranslate;
