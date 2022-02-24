import { useNavigate, useSearchParams } from "react-router-dom";
import { getCookieValue } from '../../utils/globals';
import React, { useEffect } from "react";
import { API_ROUTE } from "../..";

const sendAuthCode = async (authCode: string): Promise<boolean> => {
    const response = await fetch(API_ROUTE + '/auth/github?code=' + authCode, {
        method: 'GET',
        headers: {
            'Authorization': 'Bearer ' + getCookieValue("aeris_jwt")
        }
    });

    if (!response.ok) {
        console.error(response);
        return false;
    }
    return true;
}

export default function GithubAuth() {
    const [searchParams, setSearchParams] = useSearchParams();
    const navigate = useNavigate();
    const authCode = searchParams.get('code') as string;

    useEffect(() => {
        async function sendUserCode() {
            if (authCode.trim()) {
                await sendAuthCode(authCode);
                navigate('/pipelines');
            }
        }
        sendUserCode();
    }, []);

    return(<div/>);
}
