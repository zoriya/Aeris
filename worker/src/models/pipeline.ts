export enum ServiceType {
	Twitter,
	Youtube,
	Github
};

export enum PipelineType {
	OnTweet,

	OnYtUpload,
	OnYtLike,
	OnYtPlaylistAdd,
};

export enum ReactionType {
	Tweet,
	// Github Reactions
	openPR,
	commentPR,
	closePR,
	mergePR,
	createIssue,
	commentIssue,
	closeIssue,
	createRepo,
	createPrivateRepo,
	updateDescription,
	forkRepo,
	starRepo,
	watchRepo,
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

