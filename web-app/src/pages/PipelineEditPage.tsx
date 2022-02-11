import { Box, Typography, FormGroup, FormControlLabel, Switch } from "@mui/material"
import ArrowForwardIcon from "@mui/icons-material/ArrowForward"
import PipelineActionList from "../components/PipelineActionList";
import AddBoxIcon from "@mui/icons-material/AddBox"
import DeleteIcon from "@mui/icons-material/Delete"
import { styled } from '@mui/material/styles';
import LoadingButton from '@mui/lab/LoadingButton';

import { GenericButtonProps } from "./../components/GenericButton";

interface PipelineEditPageProps {
  title: string,
  trigger: GenericButtonProps,
  actions: Array<GenericButtonProps>
}

const Android12Switch = styled(Switch)(({ theme }) => ({
    padding: 8,
    '& .MuiSwitch-track': {
      borderRadius: 22 / 2,
      '&:before, &:after': {
        content: '""',
        position: 'absolute',
        top: '50%',
        transform: 'translateY(-50%)',
        width: 16,
        height: 16,
      },
      '&:before': {
        backgroundImage: `url('data:image/svg+xml;utf8,<svg xmlns="http://www.w3.org/2000/svg" height="16" width="16" viewBox="0 0 24 24"><path fill="${encodeURIComponent(
          theme.palette.getContrastText(theme.palette.primary.main),
        )}" d="M21,7L9,19L3.5,13.5L4.91,12.09L9,16.17L19.59,5.59L21,7Z"/></svg>')`,
        left: 12,
      },
      '&:after': {
        backgroundImage: `url('data:image/svg+xml;utf8,<svg xmlns="http://www.w3.org/2000/svg" height="16" width="16" viewBox="0 0 24 24"><path fill="${encodeURIComponent(
          theme.palette.getContrastText(theme.palette.primary.main),
        )}" d="M19,13H5V11H19V13Z" /></svg>')`,
        right: 12,
      },
    },
    '& .MuiSwitch-thumb': {
      boxShadow: 'none',
      width: 16,
      height: 16,
      margin: 2,
    },
  }));

export default function PipelineEditPage({ title, trigger, actions } : PipelineEditPageProps) {
    return (
        <div>
            <Box  sx={{ display: 'flex', flexDirection: 'row', alignItems: "center", justifyContent: "space-between", marginBottom:"100px"}} >
            <Typography variant="h2" noWrap align="left"  >
                { title }
            </Typography>
            <FormGroup>
            <FormControlLabel
              control={<Android12Switch defaultChecked />}
              label="Activer"
            />
            </FormGroup>
          </Box>
          <Box  sx={{ display: 'flex', flexDirection: 'row', alignItems: "center"}} >
            <PipelineActionList title="Actions" actions={ [trigger] } />
            <ArrowForwardIcon sx={{ height: 38, width: 38 }} />
            <Box  sx={{ display: 'flex', flexDirection: 'column', alignItems: "center"}} >
              <PipelineActionList title="Réactions" actions={ actions } />
              <LoadingButton
                sx = {{ marginTop: "10px"}}
                color="secondary"
                loading={false}
                loadingPosition="start"
                startIcon={<AddBoxIcon />}
                variant="contained"
              >
                Ajouter une réaction
              </LoadingButton>
            </Box>
          </Box>
          <Box  sx={{ display: 'flex', flexDirection: 'row', alignItems: "center", marginTop: "10px"}} >
            <LoadingButton variant="contained" color="error" startIcon={<DeleteIcon />} loadingPosition="start" loading={false} >
              Supprimer la pipeline
            </LoadingButton>
          </Box>
        </div>
    );
}