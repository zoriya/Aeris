
import { Typography, Grid } from '@mui/material'
import GenericButton, { GenericButtonProps } from './GenericButton';

interface PipelineActionListProps {
    title: string,
    actions: Array<GenericButtonProps>

}

export default function PipelineActionList({ title, actions } : PipelineActionListProps) {
    return (
        <div>
          <Typography align='left' >
            { title }
          </Typography>
          <Grid
          container
          direction="column"
          spacing={2}
          justifyContent="flex-start"
          alignItems="flex-start"
          >
            {actions.map((el, index) => (
              <Grid item sm={10} md={10} lg={5}  xl={4} key={index} >
                <GenericButton 
                    {...el}
                    />
              </Grid>
            ))}
          </Grid>
        </div>
    );
}