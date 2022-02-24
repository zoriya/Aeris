import { pipeline } from "stream";
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
		while (c.charAt(0) == " ") {
			c = c.substring(1);
		}
		if (c.indexOf(name) == 0) {
			return c.substring(name.length, c.length);
		}
	}
	return "";
}

export const PipelineParamsToApiParam = (pipelineParams: { [key: string]: ParamsType }) => {
	return Object.fromEntries(Object.entries(pipelineParams).map((el) => [el[0], el[1].value]));
};

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
