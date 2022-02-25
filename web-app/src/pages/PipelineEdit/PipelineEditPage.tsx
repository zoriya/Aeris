import { Box, Typography, FormGroup, FormControlLabel, Switch, Grid } from "@mui/material";
import ArrowForwardIcon from "@mui/icons-material/ArrowForward";
import PipelineActionList from "../../components/PipelineActionList";
import AddBoxIcon from "@mui/icons-material/AddBox";
import DeleteIcon from "@mui/icons-material/Delete";
import LoadingButton from "@mui/lab/LoadingButton";
import { styled } from "@mui/material/styles";
import GenericButton from "../../components/GenericButton";
import { useState } from "react";

import { AppAREAType, AppPipelineInfoType, AppPipelineType, AppServiceType } from "../../utils/types";

import { GenericButtonProps } from "../../components/GenericButton";

import PipelineEditPipeline from "./PipelineEditPipeline";
import PipelineEditAREA from "./PipelineEditAREA";

interface PipelineEditProps {
	pipelineData: AppPipelineType;
	handleSave: any;
	services: Array<AppServiceType>;
	actions: Array<AppAREAType>;
	reactions: Array<AppAREAType>;
	handleQuit: any;
}

export enum PipelineEditMode {
	Pipeline,
	Action,
	Reactions
}

export default function PipelineEditPage({
	pipelineData,
	handleSave,
	services,
	actions,
	reactions,
	handleQuit,
}: PipelineEditProps) {
	const [mode, setMode] = useState<PipelineEditMode>(PipelineEditMode.Pipeline);
	const [editPipelineData, setEditPipelineData] = useState<AppPipelineType>(pipelineData);
	const [editActionData, setEditActionData] = useState<AppAREAType>(pipelineData.action);
	const [editReactionsData, setEditReactionsData] = useState<Array<AppAREAType>>(pipelineData.reactions);
	const [editReactionIndex, setEditReactionIndex] = useState<number>(0);

	switch (mode) {
		default:
		case PipelineEditMode.Pipeline:
			return (
				<PipelineEditPipeline
					pipelineData={editPipelineData}
					setEditMode={setMode}
					setPipelineData={handleSave}
					setEditReactionIndex={setEditReactionIndex}
				/>
			);
		case PipelineEditMode.Action:
			return (
				<PipelineEditAREA
					pipelineData={editPipelineData}
					setEditMode={setMode}
					setAREA={(AREA: AppAREAType) => {
						setEditPipelineData({
							...editPipelineData,
							action: AREA,
						});
						setMode(PipelineEditMode.Pipeline);
					}}
					services={services}
					AREAs={actions}
				/>
			);
		case PipelineEditMode.Reactions:
			return (
				<PipelineEditAREA
					pipelineData={editPipelineData}
					setEditMode={setMode}
					setAREA={(AREA: AppAREAType) => {
						let reactionsTmp = editPipelineData.reactions;
						reactionsTmp[editReactionIndex] = AREA;
						setEditPipelineData({
							...editPipelineData,
							reactions: reactionsTmp,
						});
						setMode(PipelineEditMode.Pipeline);
					}}
					services={services}
					AREAs={reactions}
				/>
			);
	}
}
