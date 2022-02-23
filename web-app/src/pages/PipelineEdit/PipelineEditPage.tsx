import { Box, Typography, FormGroup, FormControlLabel, Switch, Grid } from "@mui/material";
import ArrowForwardIcon from "@mui/icons-material/ArrowForward";
import PipelineActionList from "../../components/PipelineActionList";
import AddBoxIcon from "@mui/icons-material/AddBox";
import DeleteIcon from "@mui/icons-material/Delete";
import LoadingButton from "@mui/lab/LoadingButton";
import { styled } from "@mui/material/styles";
import GenericButton from "../../components/GenericButton";
import { useState } from "react";


import { AppAREAType, AppPipelineInfoType, AppPipelineType, AppServiceType } from "../../utils/types"

import { GenericButtonProps } from "../../components/GenericButton";

import PipelineEditPipeline from "./PipelineEditPipeline";
import PipelineEditAREA from "./PipelineEditAREA"

interface PipelineEditProps {
	pipelineData: AppPipelineType,
	setPipelineData: any,
	services: Array<AppServiceType>,
	actions: Array<AppAREAType>,
	reactions: Array<AppAREAType>,
	handleQuit: any
}

export enum PipelineEditMode {
	Pipeline,
	Action,
	Reactions,
	SaveEdit,
	QuitEdit
}

export default function PipelineEditPage( { pipelineData, setPipelineData, services, actions, reactions, handleQuit } : PipelineEditProps) {
	const [mode, setMode] = useState<PipelineEditMode>(PipelineEditMode.Pipeline);
	const [editPipelineData, setEditPipelineData] = useState<AppPipelineType>(pipelineData);
	const [editActionData, setEditActionData] = useState<AppAREAType>(pipelineData.action);
	const [editReactionsData, setEditReactionsData] = useState<Array<AppAREAType>>(pipelineData.reactions);
	const [editReactionIndex, setEditReactionIndex] = useState<number>(0);

	switch (mode) {
		default:
		case PipelineEditMode.Pipeline:
			return <PipelineEditPipeline 
						pipelineData={editPipelineData} 
						setEditMode={setMode}
						setPipelineData={setPipelineData} 
						setEditReactionIndex={setEditReactionIndex} />
		case PipelineEditMode.Action:
			return <PipelineEditAREA 
						pipelineData={editPipelineData} 
						setEditMode={setMode} 
						setAREA={(AREA: AppAREAType) => {
							setEditActionData(AREA);
							setMode(PipelineEditMode.Pipeline);
						}} 
						services={services}
						AREAs={actions} />
		case PipelineEditMode.Reactions:
			return <PipelineEditAREA 
						pipelineData={editPipelineData} 
						setEditMode={setMode} 
						setAREA={(AREA: AppAREAType) => alert("pas de sauvegarde pour les réactions")} 
						services={services}
						AREAs={reactions} />
		case PipelineEditMode.QuitEdit:
			handleQuit();
			return <div></div>;
		case PipelineEditMode.SaveEdit:
			setEditPipelineData({
				name: pipelineData.name,
				data: pipelineData.data,
				onClickCallback: pipelineData.onClickCallback,
				action: editActionData,
				reactions: editReactionsData
			} as AppPipelineType);
			return <div></div>;

	}
}
