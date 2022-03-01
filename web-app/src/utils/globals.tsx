import { GenericButtonProps } from "../components/GenericButton";
import {
	AppAREAType,
	ReactionTypeEnum,
	ActionTypeEnum,
	ParamTypeEnum,
	AppPipelineType,
	ImageProps,
	AppServiceType,
} from "./types";
import { generateRandomString } from "./utils";
import { API_ROUTE } from "..";

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

export const AppServices: Array<AppServiceType> = [
	{
		label: "YouTube",
		uid: "youtube",
		logo: AppServicesLogos["youtube"],
		urlAuth: `https://accounts.google.com/o/oauth2/v2/auth?response_type=code&client_id=${
			process.env.REACT_APP_GOOGLE_CLIENT_ID
		}&scope=openid%20email&redirect_uri=http://localhost:3000/authorization/google&state=${generateRandomString()}`,
		linked: false,
	},
	{
		label: "Spotify",
		uid: "spotify",
		logo: AppServicesLogos["spotify"],
		urlAuth: `https://accounts.spotify.com/authorize?client_id=${process.env.REACT_APP_SPOTIFY_CLIENT_ID}&response_type=code&redirect_uri=https://localhost:3000/authorization/spotify`,
		linked: false,
	},
	{
		label: "GitHub",
		uid: "github",
		logo: AppServicesLogos["github"],
		urlAuth: `https://github.com/login/oauth/authorize?client_id=${process.env.REACT_APP_GITHUB_CLIENT_ID}&response_type=code&redirect_uri=http://localhost:3000/authorization/github`,
		linked: false,
	},
	{
		label: "Twitter",
		uid: "twitter",
		logo: AppServicesLogos["twitter"],
		urlAuth: `https://twitter.com/i/oauth2/authorize?response_type=code&client_id=${
			process.env.REACT_APP_TWITTER_CLIENT_ID
		}&redirect_uri=https://localhost:3000/authorization/twitter&scope=tweet.read%20users.read%20offline.access&state=${generateRandomString()}&code_challenge=challenge&code_challenge_method=plain`,
		linked: true,
	},
	{
		label: "Discord",
		uid: "discord",
		logo: AppServicesLogos["discord"],
		urlAuth: `https://discord.com/api/oauth2/authorize?response_type=code&client_id=${
			process.env.REACT_APP_DISCORD_CLIENT_ID
		}&scope=applications.commands%20applications.entitlements%20applications.store.update%20bot%20guilds%20guilds.join%20guilds.members.read%20identify%20messages.read%20webhook.incoming&state=${generateRandomString()}`,
		linked: true,
	},
	{
		label: "AniList",
		uid: "anilist",
		logo: AppServicesLogos["anilist"],
		urlAuth: "",
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

export const FakeAREA: AppAREAType = {
	type: "Twiitersmth",
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
		error: false,
	},
};
