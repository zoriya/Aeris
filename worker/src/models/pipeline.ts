export enum PipelineType {
	Twitter_OnTweet,

	Youtube_OnUpload
};

export enum ReactionType {
	Twitter_Tweet
};


export class Pipeline {
	id: number;
	type: PipelineType;
	name: string;
	params: {[key: string]: any};
	userID: number;
	lastTrigger: Date;
	triggerCount: number;
	enabled: boolean;

	reactions: [Reaction];
};

export class Reaction {
	id: number;
	type: ReactionType;
	params: {[key: string]: any};
};

export class PipelineEnv {
	[key: string]: any;
};

