import { useNavigate, useSearchParams } from "react-router-dom";
import React, { useEffect } from "react";
import { API_ROUTE } from "../..";

const getCookieValue = (name: string): string => {
    var nameEQ = name + "=";
    var ca = document.cookie.split(';');
    for(var i=0;i < ca.length;i++) {
        var c = ca[i].trim();
        if (c.indexOf(nameEQ) == 0)
            return c.substring(nameEQ.length,c.length);
    }
    return "";
}

const sendAuthCode = async (authCode: string): Promise<boolean> => {
    const response = await fetch(API_ROUTE + '/auth/github?code=' + authCode, {
        method: 'GET',
        headers: {
            'Authorization': 'Bearer ' + getCookieValue("aeris_jwt")
        }
    });

    if (!response.ok) {
        console.error(await response.json());
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
