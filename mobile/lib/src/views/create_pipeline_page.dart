import 'package:flutter/material.dart';
import 'package:form_builder_validators/form_builder_validators.dart';
import 'package:mobile/src/models/pipeline.dart';
import 'package:mobile/src/models/reaction.dart';
import 'package:mobile/src/models/service.dart';
import 'package:mobile/src/models/trigger.dart';
import 'package:mobile/src/providers/pipelines_provider.dart';
import 'package:mobile/src/widgets/aeris_card_page.dart';
import 'package:flutter_form_builder/flutter_form_builder.dart';
import 'package:provider/provider.dart';

/// Page to create a new pipeline
class CreatePipelinePage extends StatefulWidget {
  const CreatePipelinePage({Key? key}) : super(key: key);

  @override
  State<CreatePipelinePage> createState() => _CreatePipelinePageState();
}

class _CreatePipelinePageState extends State<CreatePipelinePage> {
  @override
  Widget build(BuildContext context) {
    final _formKey = GlobalKey<FormBuilderState>();
    return Consumer<PipelineProvider>(
        builder: (context, provider, _) => AerisCardPage(
                body: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                  const Text("Create a new pipeline",
                      style: TextStyle(
                        fontSize: 25,
                      )),
                  FormBuilder(
                      key: _formKey,
                      child: Padding(
                        padding: const EdgeInsets.all(20),
                        child: Column(children: [
                          FormBuilderTextField(
                            name: 'name',
                            decoration: const InputDecoration(
                              labelText: 'Name of the pipeline',
                            ),
                            validator: FormBuilderValidators.compose([
                              FormBuilderValidators.required(context),
                              FormBuilderValidators.minLength(context, 5),
                            ]),
                          ),
                          ElevatedButton(
                            child: const Text("Save"),
                            onPressed: () {
                              _formKey.currentState!.save();
                              if (_formKey.currentState!.validate()) {
                                print(_formKey.currentState!.value);
                                provider.addPipelineInProvider(Pipeline(
                                    id: 0,
                                    name: _formKey.currentState!.value['name'],
                                    triggerCount: 0,
                                    enabled: true,
                                    parameters: {},
                                    trigger: Trigger(
                                        service: Service.youtube(),
                                        action: 'template'),
                                    reactions: [Reaction(service: Service.github(), name: 'lol')]));
                                Navigator.of(context).pop();
                              }
                            },
                          ),
                        ]),
                      )),
                ])));
  }
}
