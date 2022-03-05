import { useNavigate, useSearchParams } from "react-router-dom";
import {sendServiceAuthToken, setCookie, signInService} from "../../utils/utils";
import { useEffect } from "react";

interface ServiceAuthProps {
    service: string
    endpoint: string
    redirect_uri: string
    navigate_to: string
}

export default function ServiceSignIn({ service, endpoint, redirect_uri, navigate_to }: ServiceAuthProps) {
    const [searchParams] = useSearchParams();
    const navigate = useNavigate();
    const authCode = searchParams.get("code") as string;

    useEffect(() => {
        signInService(authCode, "/auth/" + service + endpoint, `${window.location.origin}/${redirect_uri}`).then((ok) => {
            if (ok)
                navigate(navigate_to);
            else
                console.warn('An error occurred when signing in with a service.');
        })
    }, []);

    return <div />;
}