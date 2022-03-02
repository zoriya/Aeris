export enum ServiceType {
	Twitter,
	Youtube,
	Github,
	Spotify,
	Discord,
	Anilist,
};


export enum PipelineType {
	// Special value that will never emit an action. It is used for deleted pipelines.
	Never,
	OnTweet,

	OnYtUpload,
	OnYtLike,
	OnYtPlaylistAdd,

	OnOpenPR,
	OnCommentPR,
	OnClosePR,
	OnMergePR,
	OnCreateIssue,
	OnCommentIssue,
	OnCloseIssue,
	OnForkRepo,
	OnStarRepo,
	OnWatchRepo,
	OnSpotifyAddToPlaylist,
	OnSpotifySaveToLibrary,

	OnDiscordMessage,
	OnDiscordMessageFrom,
	OnDiscordMention,
	OnNewDiscordGuildMember,
	OnDiscordGuildLeave,
};
































export enum ReactionType {
	Tweet,
	// Youtube reactions
	YtLike,
	YtComment,
	YtAddToPlaylist,
	// Github reactions
	OpenPR,
	CommentPR,
	ClosePR,
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
	//Discord
	SetDiscordStatus,
	PostDiscordDM,
	LeaveDiscordServer,
	PostDiscordMessage,
	Pause,
	// Anilist
	ToggleFavourite,
	UpdateAbout,
	// Twitter
	followUser,
	postTweet,
	replyToTweet,
	likeTweet,
	retweet
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
