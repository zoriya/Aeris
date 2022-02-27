import { AppPipelineType } from "./types";
import { getCookie, PipeLineHostToApi } from "./utils";
import { API_ROUTE } from "..";

export const requestCreatePipeline = async (pipelineData: AppPipelineType, creation: boolean): Promise<boolean> => {
	const jwt = getCookie("aeris_jwt");

	const request = API_ROUTE + "/workflow/" + (!creation ? pipelineData.id : "") 

	const rawResponse = await fetch(request, {
		method:  creation ? "POST" : "PUT",
		headers: {
			Accept: "application/json",
			"Content-Type": "application/json",
			Authorization: "Bearer " + jwt,
		},
		body: JSON.stringify(PipeLineHostToApi(pipelineData)),
	});
	return rawResponse.ok
};

export const deletePipeline = async (pipelineData: AppPipelineType): Promise<boolean> => {
	const jwt = getCookie("aeris_jwt");

	const request = API_ROUTE + "/workflow/" + pipelineData.id

	const rawResponse = await fetch(request, {
		method:  "DELETE",
		headers: {
			Accept: "application/json",
			"Content-Type": "application/json",
			Authorization: "Bearer " + jwt,
		}
	});
	return rawResponse.ok
};