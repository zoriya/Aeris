import IconButton from "@mui/material/IconButton";
import CloseIcon from "@material-ui/icons/Close";
import { useTranslation } from 'react-i18next';
import Snackbar from '@mui/material/Snackbar';
import Button from "@material-ui/core/Button";
import React, {useState} from "react";
import '../i18n/config';
import {Tooltip} from "@mui/material";

export default function LanguageSelector() {
    const { t, i18n } = useTranslation();
    const [open, OpenSnackbar] = useState<boolean>(false);

    const changeLanguage = (language: string) => {
        i18n.changeLanguage(language);
        OpenSnackbar(true);
    }

    const handleClose = (event: React.SyntheticEvent | Event, reason?: string) => {
        if (reason === 'clickaway') return;
        OpenSnackbar(false);
    };

    const action = (
        <React.Fragment>
            <IconButton
                size="small"
                aria-label="close"
                color="inherit"
                onClick={handleClose}
            >
                <CloseIcon fontSize="small" />
            </IconButton>
        </React.Fragment>
);

    return (
        <div>
            <Tooltip title={t('switch_to_french') as string}>
                <Button onClick={() => {
                    changeLanguage('fr');
                }}>
                    <img loading="lazy" width="20"
                     src="https://flagicons.lipis.dev/flags/4x3/fr.svg"/>
                </Button>
            </Tooltip>
            <Tooltip title={t('switch_to_english') as string}>
                <Button onClick={() => {
                    changeLanguage('en');
                }}>
                    <img loading="lazy" width="20"
                         src="https://flagicons.lipis.dev/flags/4x3/gb.svg"/>
                </Button>
            </Tooltip>
            <Snackbar
                open={open}
                autoHideDuration={1000}
                onClose={handleClose}
                message={t('switch_language')}
            />
        </div>
    )
}