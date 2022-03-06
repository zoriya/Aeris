import {
	AppAREAType,
	ReactionTypeEnum,
	ActionTypeEnum,
	ParamTypeEnum,
	AppPipelineType,
	ImageProps,
	AppServiceType,
	AlertLevel,
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
	reddit: {
		imageSrc: "https://cdn.worldvectorlogo.com/logos/reddit-4.svg",
		altText: "Reddit logo",
	},
	anilist: {
		imageSrc: "https://anilist.co/img/icons/safari-pinned-tab.svg",
		altText: "AniList logo",
	},
	utils: {
		imageSrc: "https://upload.wikimedia.org/wikipedia/commons/thumb/a/af/Cle.png/1024px-Cle.png",
		altText: "Utils logo",
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
		label: "Reddit",
		uid: "reddit",
		logo: AppServicesLogos["reddit"],
		urlAuth: getServiceUrl("reddit"),
		linked: false,
	},
	{
		label: "AniList",
		uid: "anilist",
		logo: AppServicesLogos["anilist"],
		urlAuth: getServiceUrl("anilist"),
		linked: false,
	},
	{
		label: "Utils",
		uid: "utils",
		logo: AppServicesLogos["utils"],
		urlAuth: "",
		linked: true,
	},
];

export const NoAREA: AppAREAType = {
	type: "WebFrontEndNoAREA",
	params: {},
	returns: {},
	description: {},
	label: {},
	service: AppServices[0],
};

export const NewEmptyPipeline: AppPipelineType = {
	id: 89,
	name: "nouvelle pipeline",
	action: NoAREA,
	reactions: [],
	data: {
		alertLevel: AlertLevel.None,
		enabled: true,
		status: "",
		triggerCount: 0,
		lastTrigger: undefined,
		caBeEnabled: true,
	},
};
