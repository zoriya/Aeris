import { getCookie, sendServiceAuthToken } from "../../utils/utils";
import { useNavigate, useSearchParams } from "react-router-dom";
import { useEffect } from "react";
import { API_ROUTE } from "../..";

export default function SpotifyAuth() {
	const [searchParams, setSearchParams] = useSearchParams();
	const navigate = useNavigate();

	const authCode = searchParams.get("code") as string;

	useEffect(() => {
		async function sendSpotifyCode() {
			if (authCode.trim()) {
				await sendServiceAuthToken(authCode, "/auth/spotify");
				navigate("/pipelines");
			}
		}
		sendSpotifyCode();
	}, []);

	return <div />;
}
