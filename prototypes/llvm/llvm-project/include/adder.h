class Adder
{
    int sum;

public:
    explicit Adder();
    
    void add_value(const char* val);
    int get_value() const;
};
