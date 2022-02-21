export enum ServiceType {
	Twitter,
	Youtube,
	Github,
	Spotify,
};

export enum PipelineType {
	OnTweet,
	OnUpload,
};

export enum ReactionType {
	Tweet,
	// Github reactions
	OpenPR,
	commentPR,
	closePR,
	MergePR,
	CreateIssue,
	CommentIssue,
	CloseIssue,
	CreateRepo,
	CreatePrivateRepo,
	UpdateDescription,
	ForkRepo,
	StarRepo,
	WatchRepo,
	//Spotify reaction
	PlayTrack,
	AddTrackToLibrary,
	AddToPlaylist,
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

