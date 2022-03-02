import i18n from "i18next";
import { initReactI18next } from "react-i18next";
import translationFR from "./fr/translation.json";
import translationEN from "./en/translation.json";

import Backend from "i18next-http-backend";
import LanguageDetector from "i18next-browser-languagedetector";

export const resources = {
	fr: {
		translation: translationFR,
	},
	en: {
		translation: translationEN,
	},
} as const;

i18n
	.use(Backend)
	.use(initReactI18next)
	.init({
		lng: "en",
		fallbackLng: "en",
		interpolation: {
			escapeValue: false, // not needed for react as it escapes by default
		},
		resources,
	});
