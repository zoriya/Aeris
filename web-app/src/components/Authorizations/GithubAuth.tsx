import { getCookie, sendServiceAuthToken } from "../../utils/utils";
import { useNavigate, useSearchParams } from "react-router-dom";
import { useEffect } from "react";
import { API_ROUTE } from "../../utils/globals";

export default function GithubAuth() {
	const [searchParams, setSearchParams] = useSearchParams();
	const navigate = useNavigate();
	const authCode = searchParams.get("code") as string;

	useEffect(() => {
		sendServiceAuthToken(authCode, "/auth/github", `${window.location.origin}/authorization/github`).then((ok) => {
			navigate('/pipelines');
		});
	}, []);

	return <div />;
}
