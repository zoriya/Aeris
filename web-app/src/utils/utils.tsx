import { pipeline } from "stream";
import { API_ROUTE } from "..";
import { AppPipelineType, ParamsType } from "./types";

export function setCookie(cname: string, cvalue: string, exdays: number): void {
	const d = new Date();
	d.setTime(d.getTime() + exdays * 24 * 60 * 60 * 1000);
	let expires = "expires=" + d.toUTCString();
	document.cookie = cname + "=" + cvalue + ";" + expires + ";path=/";
}

export function getCookie(cname: string): string {
	let name = cname + "=";
	let decodedCookie = decodeURIComponent(document.cookie);
	let ca = decodedCookie.split(";");
	for (let i = 0; i < ca.length; i++) {
		let c = ca[i];
		c.trim();
		if (c.indexOf(name) == 0) {
			return c.substring(name.length, c.length);
		}
	}
	return "";
}

export const sendServiceAuthToken = async (authToken: string, serviceEndpoint: string): Promise<boolean> => {
	const response = await fetch(API_ROUTE + serviceEndpoint + "?code=" + authToken, {
		method: "GET",
		headers: {
			Authorization: "Bearer " + getCookie("aeris_jwt"),
		},
	});

	if (!response.ok) {
		console.log(response);
		return false;
	}
	return true;
};

export const PipelineParamsToApiParam = (pipelineParams: { [key: string]: ParamsType }) => {
	return Object.fromEntries(Object.entries(pipelineParams).map((el) => [el[0], el[1].value]));
};

<<<<<<< HEAD
=======
export const requestCreatePipeline = async (pipelineData: AppPipelineType, creation: boolean) => {
	const jwt = getCookie("aeris_jwt");

	const request = API_ROUTE + "/workflow/" + (creation ? pipelineData.id : "");

	const rawResponse = await fetch(API_ROUTE + "/workflow/", {
		method: creation ? "POST" : "PUT",
		headers: {
			Accept: "application/json",
			"Content-Type": "application/json",
			Authorization: "Bearer " + jwt,
		},
		body: JSON.stringify(PipeLineHostToApi(pipelineData)),
	});
	if (!rawResponse.ok) return false;
	return true;
};

>>>>>>> c8e433aa3794c3e9bd6d7060131d2bb790fc6fd0
export const PipeLineHostToApi = (pipelineData: AppPipelineType) => {
	return {
		action: {
			id: 69,
			name: pipelineData.name,
			pType: pipelineData.action.type,
			pParams: {
				contents: PipelineParamsToApiParam(pipelineData.action.params.contents),
				tag: pipelineData.action.type + "P",
			},
		},
		reactions: pipelineData.reactions.map((reac) => {
			return {
				rType: reac.type,
				rParams: {
					contents: PipelineParamsToApiParam(reac.params.contents),
					tag: reac.type + "P",
				},
			};
		}),
	};
};
