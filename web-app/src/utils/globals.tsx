import {
	AppAREAType,
	ReactionTypeEnum,
	ActionTypeEnum,
	ParamTypeEnum,
	AppPipelineType,
	ImageProps,
	AppServiceType,
} from "./types";

export const API_ROUTE = process.env.REACT_APP_API_ROUTE ?? "";

export const AppServicesLogos: { [key: string]: ImageProps } = {
	youtube: {
		imageSrc: "https://upload.wikimedia.org/wikipedia/commons/0/09/YouTube_full-color_icon_%282017%29.svg",
		altText: "YouTube logo",
	},
	spotify: {
		imageSrc: "https://upload.wikimedia.org/wikipedia/commons/8/84/Spotify_icon.svg",
		altText: "Spotify logo",
	},
	github: {
		imageSrc: "https://upload.wikimedia.org/wikipedia/commons/9/91/Octicons-mark-github.svg",
		altText: "GitHub logo",
	},
	twitter: {
		imageSrc: "https://upload.wikimedia.org/wikipedia/sco/9/9f/Twitter_bird_logo_2012.svg",
		altText: "Twitter logo",
	},
	discord: {
		imageSrc: "https://upload.wikimedia.org/wikipedia/fr/4/4f/Discord_Logo_sans_texte.svg",
		altText: "Discord logo",
	},
	anilist: {
		imageSrc: "https://anilist.co/img/icons/safari-pinned-tab.svg",
		altText: "AniList logo",
	},
};

const getServiceUrl = (service: string) =>
	`${API_ROUTE}/auth/${service}/url?redirect_uri=${window.location.origin}/authorization/${service}`;

export const AppServices: Array<AppServiceType> = [
	{
		label: "YouTube",
		uid: "google",
		logo: AppServicesLogos["youtube"],
		urlAuth: getServiceUrl("google"),
		linked: false,
	},
	{
		label: "Spotify",
		uid: "spotify",
		logo: AppServicesLogos["spotify"],
		urlAuth: getServiceUrl("spotify"),
		linked: false,
	},
	{
		label: "GitHub",
		uid: "github",
		logo: AppServicesLogos["github"],
		urlAuth: getServiceUrl("github"),
		linked: false,
	},
	{
		label: "Twitter",
		uid: "twitter",
		logo: AppServicesLogos["twitter"],
		urlAuth: getServiceUrl("twitter"),
		linked: false,
	},
	{
		label: "Discord",
		uid: "discord",
		logo: AppServicesLogos["discord"],
		urlAuth: getServiceUrl("discord"),
		linked: false,
	},
	{
		label: "AniList",
		uid: "anilist",
		logo: AppServicesLogos["anilist"],
		urlAuth: getServiceUrl("anilist"),
		linked: false,
	},
];

export const NoAREA: AppAREAType = {
	type: "WebFrontEndNoAREA",
	params: {},
	returns: {},
	description: "There's nothing",
	service: AppServices[0],
};

export const NewEmptyPipeline: AppPipelineType = {
	id: 89,
	name: "nouvelle pipeline",
	action: NoAREA,
	reactions: [],
	data: {
		enabled: true,
		status: "",
		errorText: "",
		error: false,
		triggerCount: 0,
		lastTrigger: new Date(),
	},
};
