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
	description: string;
	type: ParamTypeEnum;
}

export interface AppAREAType {
	isAction: boolean;
	type: ActionTypeEnum | ReactionTypeEnum;
	params: {
		contents: { [key: string]: ParamsType };
	};
	description?: string;
	service: AppServiceType;
	onClickCallback?: React.MouseEventHandler<HTMLButtonElement>;
}

export interface AppPipelineInfoType {
	enabled: boolean;
	status: string;
	error: boolean;
}

export interface AppPipelineType {
	id: number;
	name: string;
	action: AppAREAType;
	reactions: Array<AppAREAType>;
	data: AppPipelineInfoType;
	onClickCallback?: React.MouseEventHandler<HTMLButtonElement>;
}
