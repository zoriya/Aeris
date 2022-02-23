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
    urlAuth: string,
    linked: boolean
}

export enum ParamType {
    Bool = "boolean",
    String = "string",
    StringList = "stringList"
}

export enum ActionTypeEnum {
    None = "None",
    TwitterNewPost = "TwitterNewPost"
}

export enum ReactionTypeEnum {
    None = "None",
    TwitterTweet = "TwitterTweet"
}

export interface ParamsType {
    value: string,
    description: string,
    type: ParamType

}

export interface AppActionType {
    type: ActionTypeEnum,
    params: {
        contents: { [key:string]: ParamsType }
    }
    description?: string,
    service: AppServiceType,
    onClickCallback?: React.MouseEventHandler<HTMLButtonElement>
}

export interface AppReactionType {
    type: ReactionTypeEnum,
    params: {
        contents: { [key:string]: ParamsType }
    },
    description?: string,
    service: AppServiceType,
    onClickCallback?: React.MouseEventHandler<HTMLButtonElement>
}

export interface AppPipelineInfoType {
    enabled: boolean,
    status: string,
    error: boolean
}

export interface AppPipelineType {
    name: string,
    action: AppActionType,
    reactions: Array<AppReactionType>,
    data: AppPipelineInfoType,
    onClickCallback?: React.MouseEventHandler<HTMLButtonElement>
}