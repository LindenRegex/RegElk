using System;
using System.Text.RegularExpressions;

string pattern = Environment.GetCommandLineArgs()[1];
string input = Environment.GetCommandLineArgs()[2];
Regex rgx = new Regex(pattern);
int[] groupNumbers = rgx.GetGroupNumbers();
Match m = rgx.Match(input);

if (m.Success) {
    // Console.WriteLine("Match: {0}", m.Value);
    foreach (var groupNumber in groupNumbers) {
	if (m.Groups[groupNumber].Success) {
	    Console.WriteLine("#{0}:{1}",  groupNumber, m.Groups[groupNumber].Value);
	}
	else {
	    Console.WriteLine("#{0}:Undefined",  groupNumber);
	}
    }   
}
else {
    Console.WriteLine("NoMatch");
}
Console.WriteLine("");
