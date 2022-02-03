import 'package:flutter/material.dart';
import 'package:form_builder_validators/form_builder_validators.dart';
import 'package:mobile/src/models/pipeline.dart';
import 'package:mobile/src/models/reaction.dart';
import 'package:mobile/src/models/service.dart';
import 'package:mobile/src/models/trigger.dart';
import 'package:mobile/src/providers/pipelines_provider.dart';
import 'package:mobile/src/views/pipeline_detail_page.dart';
import 'package:mobile/src/views/setup_action_page.dart';
import 'package:mobile/src/widgets/aeris_card_page.dart';
import 'package:flutter_form_builder/flutter_form_builder.dart';
import 'package:mobile/src/widgets/clickable_card.dart';
import 'package:mobile/src/widgets/colored_clickable_card.dart';
import 'package:mobile/src/widgets/warning_dialog.dart';
import 'package:provider/provider.dart';

/// Page to create a new pipeline
class CreatePipelinePage extends StatefulWidget {
  const CreatePipelinePage({Key? key}) : super(key: key);

  @override
  State<CreatePipelinePage> createState() => _CreatePipelinePageState();
}

class _CreatePipelinePageState extends State<CreatePipelinePage> {
  Trigger trigger = Trigger.template();
  List<Reaction> reactions = [];
  String? name;

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
                            initialValue: name,
                            decoration: const InputDecoration(
                              labelText: 'Name of the pipeline',
                            ),
                            validator: FormBuilderValidators.compose([
                              FormBuilderValidators.required(context),
                              FormBuilderValidators.minLength(context, 5),
                            ]),
                            onChanged: (value) {
                              name = value;
                            },
                          ),
                          Padding(
                            padding: const EdgeInsets.all(8.0),
                            child: ColoredClickableCard(
                                color: Theme.of(context)
                                    .colorScheme
                                    .secondaryContainer,
                                text: "Add Trigger",
                                onTap: () {
                                  print("add trigger"); // TODO add reaction
                                  Navigator.of(context)
                                      .pushNamed('/pipeline/action/new',
                                          arguments:
                                              SetupActionPageArguments(trigger))
                                      .then((_) => setState(() {}));
                                }),
                          ),
                          Padding(
                            padding: const EdgeInsets.all(8.0),
                            child: ColoredClickableCard(
                                color: Theme.of(context)
                                    .colorScheme
                                    .secondaryContainer,
                                text: "Add Reaction",
                                onTap: () async {
                                  print("add reaction"); // TODO add reaction
                                  reactions.add(Reaction.template());
                                  await Navigator.of(context).pushNamed(
                                      '/pipeline/action/new',
                                      arguments: SetupActionPageArguments(
                                          reactions.last));
                                  setState(() {});
                                }),
                          ),
                          ElevatedButton(
                            child: const Text("Save"),
                            onPressed: () {
                              _formKey.currentState!.save();
                              if (_formKey.currentState!.validate()) {
                                if (trigger == Trigger.template() ||
                                    reactions.isEmpty ||
                                    reactions
                                        .where((element) =>
                                            element == Reaction.template())
                                        .isNotEmpty) {
                                  ///TODO Warning
                                } else {
                                  Pipeline newPipeline = Pipeline(
                                      id: 0,
                                      name:
                                          _formKey.currentState!.value['name'],
                                      triggerCount: 0,
                                      enabled: true,
                                      parameters: {},
                                      trigger: trigger,
                                      reactions: reactions);
                                  provider.addPipelineInProvider(newPipeline);
                                  Navigator.of(context).popAndPushNamed(
                                      '/pipeline',
                                      arguments: PipelineDetailPageArguments(
                                          newPipeline));
                                }
                              }
                            },
                          ),
                        ]),
                      )),
                ])));
  }
}
