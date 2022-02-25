import { getCookie, sendServiceAuthToken } from "../../utils/utils";
import { useNavigate, useSearchParams } from "react-router-dom";
import React, { useEffect } from "react";
import { API_ROUTE } from "../..";

export default function GithubAuth() {
	const [searchParams, setSearchParams] = useSearchParams();
	const navigate = useNavigate();
	const authCode = searchParams.get("code") as string;

	useEffect(() => {
		sendServiceAuthToken(authCode, "/auth/github").then((ok) => {
			navigate('/pipelines');
		});
	}, []);

	return <div />;
}
