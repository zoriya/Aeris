import { useNavigate } from "react-router-dom";
import React, { useEffect } from "react";
import { API_ROUTE } from "../..";

const sendAuthCode = async (authCode: string): Promise<boolean> => {
    const response = await fetch(API_ROUTE + '/auth/github?code=' + authCode, {
        method: 'POST',
        headers: {
            'Accept': 'application/json',
            'Authorization': 'Bearer ' + authCode
        }
    });

    if (!response.ok) {
        console.error(await response.json());
        return false;
    }
    return true;
}

export default function GithubAuth({authCode}: {authCode: string}) {
    const navigate = useNavigate();

    async function sendUserCode() {
        if (authCode.trim())
            if (await sendAuthCode(authCode))
                navigate('/pipelines');
    }

    return(<div/>);
}
