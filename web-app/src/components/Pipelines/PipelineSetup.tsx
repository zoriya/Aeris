import PipelineModal from './PipelineModal';

import {Google, GitHub, MusicNote, Twitter, YouTube} from '@mui/icons-material';

import ListItemIcon from '@mui/material/ListItemIcon';
import ListItemText from '@mui/material/ListItemText';
import Typography from '@mui/material/Typography';
import MenuItem from '@mui/material/MenuItem';
import Select from '@mui/material/Select';
import Box from '@mui/material/Box';

interface PipelineSetupPageProps {
    actionName: string
}

export default function PipelineSetupModal({ actionName } : PipelineSetupPageProps) {

    //TODO On line 23, need to change number 11 to number of available actions
    return (
        <div>
            <Box
                sx={{ display: 'flex', flexDirection: 'row', alignItems: "center", justifyContent: "space-between", marginBottom:"100px"}}
            >
                <Typography variant="h2" noWrap align="left">
                    Setup Action: { actionName }
                </Typography>
            </Box>
            <Box
                sx={{ display: 'flex', flexDirection: 'row', alignItems: "right" }}
            >
                <Typography variant="h3" noWrap align="right">
                    11 available actions for
                </Typography>
                <Select>
                    <MenuItem value="twitter">
                        <ListItemIcon>
                            <Twitter />
                        </ListItemIcon>
                        <ListItemText primary="Twitter" />
                    </MenuItem>
                    <MenuItem value="youtube">
                        <ListItemIcon>
                            <YouTube />
                        </ListItemIcon>
                        <ListItemText primary="Youtube" />
                    </MenuItem>
                    <MenuItem value="github">
                        <ListItemIcon>
                            <GitHub />
                        </ListItemIcon>
                        <ListItemText primary="Github" />
                    </MenuItem>
                    <MenuItem value="spotify">
                        <ListItemIcon>
                            <MusicNote />
                        </ListItemIcon>
                        <ListItemText primary="Spotify" />
                    </MenuItem>
                    <MenuItem value="google">
                        <ListItemIcon>
                            <Google />
                        </ListItemIcon>
                        <ListItemText primary="Google Gmail" />
                    </MenuItem>
                </Select>
            </Box>
        </div>
    )
}