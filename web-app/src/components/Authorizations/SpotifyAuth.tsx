import { getCookie, sendServiceAuthToken } from "../../utils/utils";
import { useNavigate, useSearchParams } from "react-router-dom";
import { useEffect } from "react";
import { API_ROUTE } from "../../utils/globals";

export default function SpotifyAuth() {
	const [searchParams, setSearchParams] = useSearchParams();
	const navigate = useNavigate();

	const authCode = searchParams.get("code") as string;

	useEffect(() => {
		sendServiceAuthToken(authCode, "/auth/spotify", `${window.location.origin}/authorization/spotify`).then((ok) => {
			navigate('/pipelines');
		});
	}, []);

	return <div />;
}
