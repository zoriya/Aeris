import { getCookie, sendServiceAuthToken } from '../../utils/utils';
import { useNavigate, useSearchParams } from 'react-router-dom';
import { useEffect } from "react";
import { API_ROUTE } from "../..";

export default function DiscordAuth() {
    const [searchParams, setSearchParams] = useSearchParams();
    const navigate = useNavigate();

    const authToken = searchParams.get('code') as string;

    useEffect(() => {
        async function sendDiscordCode() {
            if (authToken.trim()) {
                await sendServiceAuthToken(authToken, '/auth/discord');
                navigate('/pipelines');
            }
        }
        sendDiscordCode();
    }, []);

    return (<div/>);
}