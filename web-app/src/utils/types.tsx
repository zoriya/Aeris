export enum ParamType {
    Bool = "boolean",
    String = "string",
    StringList = "stringList"
}

export enum ActionTypeEnum {
    TwitterNewPost = "TwitterNewPost"
}

export enum ReactionTypeEnum {
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
        tag: string,
        contents: { [key:string]: ParamsType }
    }
    description?: string
}

export interface AppReactionType {
    type: ReactionTypeEnum,
    params: {
        tag: string,
        contents: { [key:string]: ParamsType }
    },
    description?: string
}

export interface AppPipelineInfoType {
    enabled: boolean,
    status: string,
    error: boolean
}

export interface AppPipelineType {
    name: string,
    action: AppActionType,
    reactions: Array<AppActionType>,
    data: AppPipelineInfoType
}