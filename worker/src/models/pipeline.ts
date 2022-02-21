export enum ServiceType {
	Twitter,
	Youtube,
};

export enum PipelineType {
	OnTweet,

	OnYtUpload,
	OnYtLike,
	OnYtPlaylistAdd,
};


export enum ReactionType {
	Tweet,
	YtLike,
	YtComment,
};


export class Pipeline {
	id: number;
	service: ServiceType;
	type: PipelineType;
	name: string;
	params: {[key: string]: string};
	userData: {[key: string]: any};
	lastTrigger: Date;
	triggerCount: number;
	enabled: boolean;

	reactions: [Reaction];
};

export class Reaction {
	id: number;
	service: ServiceType;
	type: ReactionType;
	params: {[key: string]: any};
};

export class PipelineEnv {
	[key: string]: any;
};

