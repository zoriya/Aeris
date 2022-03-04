export interface ImageProps {
	// the image src preferable to use svg files
	imageSrc: string;
	// the alt text (screen readers, etc)
	altText: string;
}

export interface AppServiceType {
	label: string;
	uid: string;
	logo: ImageProps;
	urlAuth: string;
	linked: boolean;
}

export enum ParamTypeEnum {
	Bool = "boolean",
	String = "string",
	StringList = "stringList",
}

export enum ActionTypeEnum {
	None = "None",
	Changed = "Changed",
	TwitterNewPost = "TwitterNewPost",
}

export enum ReactionTypeEnum {
	None = "None",
	Changed = "Changed",
	TwitterTweet = "TwitterTweet",
}

export interface ParamsType {
	value: string;
	description: { [key: string]: string };
	type: ParamTypeEnum;
}

export interface AppAREAType {
	type: ActionTypeEnum | ReactionTypeEnum | string;
	label: { [key: string]: string };
	params: { [key: string]: ParamsType };
	returns: { [key: string]: { [key: string]: string } };
	description: { [key: string]: string };
	service: AppServiceType;
}

export enum AlertLevel {
	None = "none",
	Info = "info",
	Warning = "warning",
	Error = "error",
	Success = "success",
}

export interface AppPipelineInfoType {
	enabled: boolean;
	status: string;
	triggerCount: number;
	lastTrigger: Date;
	alertLevel: AlertLevel;
}

export interface AppPipelineType {
	id: number;
	name: string;
	action: AppAREAType;
	reactions: Array<AppAREAType>;
	data: AppPipelineInfoType;
}
