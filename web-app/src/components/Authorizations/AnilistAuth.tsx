import { getCookie, sendServiceAuthToken } from "../../utils/utils";
import { useNavigate, useSearchParams } from "react-router-dom";
import React, { useEffect } from "react";
import { API_ROUTE } from "../../utils/globals";

export default function Anilist() {
	const [searchParams, setSearchParams] = useSearchParams();
	const navigate = useNavigate();
	const authCode = searchParams.get("code") as string;

	useEffect(() => {
		sendServiceAuthToken(authCode, "/auth/anilist", `${window.location.origin}/authorization/anilist`).then((ok) => {
			navigate('/pipelines');
		});
	}, []);

	return <div />;
}
