import { pipeline } from "stream";
import { API_ROUTE } from "..";
import { AppServices } from "./globals";
import { AppAREAType, AppPipelineType, AppServiceType, ParamsType } from "./types";

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
		let c = ca[i].trim();
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

	return response.ok;
};

export const PipelineParamsToApiParam = (pipelineParams: { [key: string]: ParamsType }) => {
	return Object.fromEntries(Object.entries(pipelineParams).map((el) => [el[0], el[1].value]));
};

export const PipeLineHostToApi = (pipelineData: AppPipelineType) => {
	return {
		action: {
			id: 69,
			name: pipelineData.name,
			enabled: pipelineData.data.enabled,
			pType: pipelineData.action.type,
			pParams: {
				contents: PipelineParamsToApiParam(pipelineData.action.params),
				tag: pipelineData.action.type + "P",
			},
		},
		reactions: pipelineData.reactions.map((reac) => {
			return {
				rType: reac.type,
				rParams: {
					contents: PipelineParamsToApiParam(reac.params),
					tag: reac.type + "P",
				},
			};
		}),
	};
};

const deSerializeAREAParams = (dumpAREAParam: Array<any>): { [key: string]: ParamsType } => {
	let params: { [key: string]: ParamsType } = {};
	dumpAREAParam.forEach((el) => {
		params[el.name] = {
			value: "",
			type: el.type,
			description: el.description,
		};
	});
	return params;
};

const deSerializeAREAReturns = (dumpAREAReturns: Array<any>): { [key: string]: string } => {
	let returns: { [key: string]: string } = {};
	dumpAREAReturns.forEach((el) => {
		returns[el.name] = el.description;
	});
	return returns;
};

export const deSerializeAREA = (dumpAREA: any, service: AppServiceType): AppAREAType => {
	return {
		type: dumpAREA.name,
		description: dumpAREA.description,
		service: service,
		params: deSerializeAREAParams(dumpAREA.params),
		returns: deSerializeAREAReturns(dumpAREA.returns),
	};
};

export const deSerializeService = (dumpService: any, services: Array<AppServiceType>): Array<Array<AppAREAType>> => {
	let service: AppServiceType = services.filter((el) => el.uid === dumpService.name.toLowerCase())[0] ?? services[0];

	let actions: Array<AppAREAType> = dumpService.actions.map((el: any) => deSerializeAREA(el, service));
	let reactions: Array<AppAREAType> = dumpService.reactions.map((el: any) => deSerializeAREA(el, service));

	return [actions, reactions];
};

export const deSerializeServices = (
	dumpServices: Array<any>,
	services: Array<AppServiceType>
): Array<Array<AppAREAType>> => {
	let actions: Array<AppAREAType> = [];
	let reactions: Array<AppAREAType> = [];

	dumpServices.forEach((serviceData) => {
		let newAREAs = deSerializeService(serviceData, services);
		actions = actions.concat(newAREAs[0]);
		reactions = reactions.concat(newAREAs[1]);
	});
	return [actions, reactions];
};

export const deSerialiseApiPipelineAction = (data: any, actions: Array<AppAREAType>): AppAREAType => {
	const refAction = actions.filter((el) => el.type === data.pType);

	return {
		...refAction[0],
		params: data.pParams.contents,
	};
};

export const deSerialiseApiPipelineReaction = (data: any, reactions: Array<AppAREAType>): AppAREAType => {
	const refReaction = reactions.filter((el) => el.type === data.rType);

	return {
		...refReaction[0],
		params: data.rParams.contents,
	};
};

export const deSerialisePipeline = (data: any, AREAs: Array<Array<AppAREAType>>): AppPipelineType => {
	let reactionList: AppAREAType[] = [];

	for (const reaction of data.reactions) {
		reactionList.push(deSerialiseApiPipelineReaction(reaction, AREAs[1]));
	}

	return {
		id: data["action"]["id"],
		name: data["action"]["name"],
		action: deSerialiseApiPipelineAction(data.action, AREAs[0]),
		reactions: reactionList,
		data: {
			enabled: data.action.enabled,
			error: false,
			status: "mdr", //TODO => Change status from request
		},
	} as AppPipelineType;
};

export const fetchWorkflows = async (): Promise<any> => {
	const response = await fetch(API_ROUTE + '/workflows', {
		method: 'GET',
		headers: {
			Accept: 'application/json',
			"Content-Type": 'application/json',
			Authorization: 'Bearer ' + getCookie('aeris_jwt')
		}
	});

	if (response.ok) {
		let json = await response.json();
		return json;
	}
	console.error("Can't fetch newer workflows");
	return null;
}