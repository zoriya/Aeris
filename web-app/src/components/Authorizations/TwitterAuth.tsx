import { getCookie, sendServiceAuthToken } from "../../utils/utils";
import { useNavigate, useSearchParams } from "react-router-dom";
import { useEffect } from "react";
import { API_ROUTE } from "../..";

export default function TwitterAuth() {
	const [searchParams, setSearchParams] = useSearchParams();
	const navigate = useNavigate();

	const authCode = searchParams.get("code") as string;

	useEffect(() => {
		async function sendTwitterCode() {
			if (authCode.trim()) {
				await sendServiceAuthToken(authCode, "/auth/twitter");
				navigate("/pipelines");
			}
		}
		sendTwitterCode();
	}, []);

	return <div />;
}
