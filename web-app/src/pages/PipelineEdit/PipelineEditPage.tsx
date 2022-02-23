import { Box, Typography, FormGroup, FormControlLabel, Switch, Grid } from "@mui/material";
import ArrowForwardIcon from "@mui/icons-material/ArrowForward";
import PipelineActionList from "../../components/PipelineActionList";
import AddBoxIcon from "@mui/icons-material/AddBox";
import DeleteIcon from "@mui/icons-material/Delete";
import LoadingButton from "@mui/lab/LoadingButton";
import { styled } from "@mui/material/styles";
import GenericButton from "../../components/GenericButton";
import { useState } from "react";


import { AppActionType, AppPipelineInfoType, AppPipelineType, AppReactionType } from "../../utils/types"

import { GenericButtonProps } from "../../components/GenericButton";

import PipelineEditPipeline from "./PipelineEditPipeline";

interface PipelineEditProps {
	pipelineData: AppPipelineType,
	setPipelineData: any
}

enum PipelineEditMode {
	Pipeline,
	Action,
	Reactions
}

export default function PipelineEditPage( { pipelineData, setPipelineData } : PipelineEditProps) {
	const [mode, setMode] = useState<PipelineEditMode>(PipelineEditMode.Pipeline);
	const [editPipelineData, setEditPipelineData] = useState<AppPipelineType>(pipelineData);

	switch (mode) {
		default:
		case PipelineEditMode.Pipeline:
			return <PipelineEditPipeline pipelineData={editPipelineData} setEditMode={setMode} setPipelineData={setPipelineData} />

	}
}
