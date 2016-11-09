#ifndef _ARGUMENTREADER_HPP
#define _ARGUMENTREADER_HPP

class ArgumentReader
{
	int argc;
	char ** argv;

public:
	ArgumentReader(int _argc, char ** _argv) :
		argc(_argc),
		argv(_argv)
	{}

	int getNumArgs()
	{
		return argc - 1;
	}

	std::string getCommand()
	{
		return argv[0];
	}

	template<class T>
	T get(int idx)
	{
		std::istringstream ss(argv[idx + 1]);
		T result;
		ss >> result;
		return result;
	}

	// will collect repeated occurrences of "-name PARAM"
	// maxTimes 0: collect all , n>0: only collect n times
	template<class T>
	bool collectOption(const std::string & name, int len, std::vector<T> & oParams, uint maxTimes = 0)
	{
		int times = maxTimes > 0 ? maxTimes : getNumArgs();

		bool readItems = false;
		oParams.clear();
		for(int start = 0; start < getNumArgs() && times > 0; ++start)
		{
			if (get<std::string>(start) == name)
			{
				readItems = true;
				times--;

				// collect
				int base = start + 1;
				for(int paramOff = 0; paramOff < len; ++paramOff)
				{
					int idx = base + paramOff;

					if (idx < getNumArgs()) {
						oParams.push_back( get<T>(idx) );
					} else {
						return readItems;
					}
				}
			}
		}

		return readItems;
	}

	// read the first occurrence of "-name"
	template<class T>
	bool readOption(const std::string & name, int len, std::vector<T> & oParams)
	{
		return collectOption<T>(name, len, oParams, 1);
	}

	template<class T>
	bool readOption(const std::string & name, T & oParam)
	{
		std::vector<T> params;
		if (readOption<T>(name, 1, params)) {
			oParam = *params.begin();
			return true;
		}
		return false;
	}

	template<class T>
	T getOption(const std::string & name,T defaultValue)
	{
		T buffer = defaultValue;
		if (readOption(name, buffer)) {
			return buffer;
		}
		return defaultValue;
	}

	bool hasOption(const std::string & name)
	{
		std::vector<int> dummies;
		return readOption<int>(name, 0, dummies);
	}
};


#endif
