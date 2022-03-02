import 'package:aeris/src/aeris_api.dart';
import 'package:flutter/material.dart';
import 'package:flutter_form_builder/flutter_form_builder.dart';
import 'package:get_it/get_it.dart';

/// Floating Action button to access the setup API route modal
class SetupAPIRouteButton extends StatefulWidget {
  const SetupAPIRouteButton({Key? key}) : super(key: key);

  @override
  State<SetupAPIRouteButton> createState() => _SetupAPIRouteButtonState();
}

class _SetupAPIRouteButtonState extends State<SetupAPIRouteButton> {
  bool? connected;

  @override
  void initState() {
    super.initState();
    GetIt.I<AerisAPI>().getAbout().then((value) {
      setState(() {
        connected = value.isNotEmpty;
      });
    });
  }
  @override
  Widget build(BuildContext context) {
    return FloatingActionButton(
      onPressed: () => showDialog(
          context: context, 
          builder: (_) => const SetupAPIRouteModal()
      ).then((_) => setState(() {
        GetIt.I<AerisAPI>().getAbout().then((value) {
          setState(() {
            connected = value.isNotEmpty;
          });
        });
      })),
      backgroundColor: Theme.of(context).colorScheme.secondary,
      elevation: 10,
      child: Icon(
        connected == true
          ? Icons.wifi
          : Icons.signal_cellular_connected_no_internet_0_bar_sharp
      ),
    );
  }
}

/// Modal to setup route to connect to api
class SetupAPIRouteModal extends StatefulWidget {
  const SetupAPIRouteModal({Key? key}) : super(key: key);

  @override
  State<SetupAPIRouteModal> createState() => _SetupAPIRouteModalState();
}

class _SetupAPIRouteModalState extends State<SetupAPIRouteModal> {
  bool? connected;
  final _formKey = GlobalKey<FormBuilderState>();

  @override
  void initState() {
    super.initState();
    GetIt.I<AerisAPI>().getAbout().then((value) {
      setState(() {
        connected = value.isNotEmpty;
      });
    });
  }

  @override
  Widget build(BuildContext context) {
    return AlertDialog(
        title: Text("Setup API Route"),///TODO translate
        content: FormBuilder(
          key: _formKey,
          child: FormBuilderTextField(
            initialValue: GetIt.I<AerisAPI>().baseRoute ,
            name: "route",
            autovalidateMode: AutovalidateMode.onUserInteraction,
            validator: (host) {
              if (host == GetIt.I<AerisAPI>().baseRoute && connected == true) {
                return;
              }
              if (Uri.tryParse(host ?? "") == null) {
                setState(() => connected = false);
              } else {
                GetIt.I<AerisAPI>().baseRoute = host;
                GetIt.I<AerisAPI>().getAbout().then((value) {
                  setState(() {
                    connected = value.isNotEmpty;
                  });
                });
              }
            },
            decoration: InputDecoration(
              labelText: "Route to API", ///TODO transalte
              helperText: "Ex: http://host:port"
            )),
        ),
        actions: [
          ElevatedButton(
            child: Text(connected == true ? "Save" : "Can't save"),
            onPressed:
                connected == true ? () => Navigator.of(context).pop() : null,
          )
        ]);
  }
}
