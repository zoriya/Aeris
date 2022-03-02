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
import MoreVert from "@mui/icons-material/MoreVert";


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

const generateRandomString = (): string => {
	let randomString = "";
	const randomNumber = Math.floor(Math.random() * 10);

	for (let i = 0; i < 20 + randomNumber; i++) {
		randomString += String.fromCharCode(33 + Math.floor(Math.random() * 94));
	}
	return randomString;
};

const getServiceUrl = (service: string) => `${API_ROUTE}/auth/${service}/url?redirect_uri=${window.location.origin}/authorization/${service}`

export const AppServices: Array<AppServiceType> = [
	{
		label: "YouTube",
		uid: "youtube",
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
		linked: true,
	},
	{
		label: "Discord",
		uid: "discord",
		logo: AppServicesLogos["discord"],
		urlAuth: getServiceUrl("discord"),
		linked: true,
	},
	{
		label: "AniList",
		uid: "anilist",
		logo: AppServicesLogos["anilist"],
		urlAuth: getServiceUrl("anilist"),
		linked: false,
	},
];

export const ServiceActions: { [key: string]: Array<GenericButtonProps> } = {
	youtube: [
		{
			title: "Une vidéo à été publiée",
			service: AppServicesLogos["youtube"],
			trailingIcon: <MoreVert />,
		},
		{
			title: "Une vidéo à été likée",
			service: AppServicesLogos["youtube"],
			trailingIcon: <MoreVert />,
		},
		{
			title: "La playlist est mise à jour",
			service: AppServicesLogos["youtube"],
			trailingIcon: <MoreVert />,
		},
	],
	spotify: [
		{
			title: "Nouvelle musique d'un artiste",
			service: AppServicesLogos["spotify"],
			trailingIcon: <MoreVert />,
		},
		{
			title: "Playlist modifiée",
			service: AppServicesLogos["spotify"],
			trailingIcon: <MoreVert />,
		},
	],
	github: [
		{
			title: "Un repository a reçu une étoile",
			service: AppServicesLogos["github"],
			trailingIcon: <MoreVert />,
		},
		{
			title: "Une Pull Request à été ouverte",
			service: AppServicesLogos["github"],
			trailingIcon: <MoreVert />,
		},
		{
			title: "Push sur un repository",
			service: AppServicesLogos["github"],
			trailingIcon: <MoreVert />,
		},
	],
	twitter: [
		{
			title: "Vous êtes harcelé",
			service: AppServicesLogos["twitter"],
			trailingIcon: <MoreVert />,
		},
		{
			title: "Un compte vient de twitter",
			service: AppServicesLogos["twitter"],
			trailingIcon: <MoreVert />,
		},
	],
	discord: [
		{
			title: "Réception de demande d'ami",
			service: AppServicesLogos["discord"],
			trailingIcon: <MoreVert />,
		},
		{
			title: "Réception d'un message privé",
			service: AppServicesLogos["discord"],
			trailingIcon: <MoreVert />,
		},
		{
			title: "Vous avez été ping",
			service: AppServicesLogos["discord"],
			trailingIcon: <MoreVert />,
		},
	],
	gmail: [
		{
			title: "Réception d'un mail",
			service: AppServicesLogos["anilist"],
			trailingIcon: <MoreVert />,
		},
	],
};

export const NoAREA: AppAREAType = {
	type: "WebFrontEndNoAREA",
	params: {
		contents: {},
	},
	returns: {},
	description: "There's nothing",
	service: AppServices[0],
};

export const FakeAREA: AppAREAType = {
	type: "Twiitersmth",
	params: {
		contents: {},
	},
	returns: {},
	description: "There's nothing",
	service: AppServices[0],
};


export const AppListPipelines: Array<AppPipelineType> = [
	{
		id: 56,
		name: "my pipe",
		action: FakeAREA,
		reactions: [FakeAREA],
		data: {
			enabled: true,
			status: "il fait beau aujourd'hui",
			error: false,
		},
	},
	{
		id: 89,
		name: "nouvelle pipeline",
		action: FakeAREA,
		reactions: [],
		data: {
			enabled: true,
			status: "ninjago",
			error: false,
		},
	},
];
