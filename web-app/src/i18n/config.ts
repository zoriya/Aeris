import i18n from 'i18next';
import { initReactI18next } from 'react-i18next';
import translationFR from './fr/translation.json';
import translationEN from './en/translation.json';

export const resources = {
    fr: {
        translation: translationFR
    },
    en: {
        translation: translationEN
    }
} as const;

i18n.use(initReactI18next).init({
    lng: 'en',
    interpolation: {
        escapeValue: false, // not needed for react as it escapes by default
    },
    resources,
});
