import i18n from 'i18next';
import { initReactI18next } from 'react-i18next';
import translation from './fr/translation.json';

export const resources = {
    fr: {
        translation
    },
} as const;

i18n.use(initReactI18next).init({
    lng: 'fr',
    interpolation: {
        escapeValue: false, // not needed for react as it escapes by default
    },
    resources,
});
