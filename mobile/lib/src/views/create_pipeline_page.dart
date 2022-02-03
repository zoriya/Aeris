import 'package:flutter/material.dart';
import 'package:form_builder_validators/form_builder_validators.dart';
import 'package:mobile/src/widgets/aeris_card_page.dart';
import 'package:flutter_form_builder/flutter_form_builder.dart';

/// Page to create a new pipeline
class CreatePipelinePage extends StatelessWidget {
  const CreatePipelinePage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    final _formKey = GlobalKey<FormBuilderState>();
    return AerisCardPage(
        body: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
      FormBuilder(
          key: _formKey,
          autovalidateMode: AutovalidateMode.always,
          child: Column(children: [
            FormBuilderTextField(
              name: 'name',
              decoration: const InputDecoration(
                labelText: 'Name of the pipeline',
              ),
              validator: FormBuilderValidators.compose([
                FormBuilderValidators.required(context),
                FormBuilderValidators.minLength(context, 5),
                ((value) => value!.trim().isEmpty ? null : value)
              ]),
            ),
          ])),
    ]));
  }
}
